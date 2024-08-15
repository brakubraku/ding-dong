{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

-- optics support
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric              #-}

module Nostr.Event where

import           Control.Monad          (mzero)
import qualified Crypto.Hash.SHA256     as SHA256
import           MyCrypto hiding (decodeHex)
import           Data.Aeson
import           Data.Aeson.Text        (encodeToTextBuilder)
import           Data.ByteString        (ByteString)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Base16 as B16
import qualified Data.Base16.Types as B16
import           Data.ByteString.Lazy   (fromStrict, toStrict)
import           Data.Time
import           Data.Default
import           Data.Maybe             (fromJust, fromMaybe)
import           Data.Text              (Text, toLower, pack, unpack)
import           Data.Text.Encoding     (encodeUtf8)
import qualified Data.Text.Lazy         as LazyText
import           Data.Text.Lazy.Builder (toLazyText)
import qualified Data.Vector            as V
import Data.List
import           GHC.Exts               (fromList)

import Nostr.Keys
import Nostr.Kind
import Nostr.Profile (Profile(..), RelayURL, Username)
import Nostr.Relay
import Debug.Trace
import Nostr.OtherUtils (decodeHex)
import Data.DateTime 
import GHC.Generics

newtype EventId = EventId
  { getEventId :: ByteString
  }
  deriving (Eq, Ord)

data Marker = Reply | Root | Mention
  deriving (Eq, Show)

data Tag
  = ETag EventId (Maybe RelayURL) (Maybe Marker)
  | PTag UnknownXOnlyPubKey (Maybe RelayURL) (Maybe ProfileName)
  | NonceTag
  | UnknownTag String
  deriving (Eq, Show)

data Event = Event
  { eventId    :: EventId
  , pubKey     :: XOnlyPubKey
  , created_at :: DateTime
  , kind       :: Kind
  , tags       :: [Tag]
  , content    :: Text
  , sig        :: Bip340Sig
  }
  deriving (Eq, Show, Generic)

data UnsignedEvent = UnsignedEvent
  { pubKey'     :: XOnlyPubKey
  , created_at' :: DateTime
  , kind'       :: Kind
  , tags'       :: [Tag]
  , content'    :: Text
  }
  deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _      -> fail "invalid event id"

-- TODO: check this extractBase is correct
instance ToJSON EventId where
  -- toJSON e = String $ B16.encodeBase16 $ getEventId e
  toJSON e = String . B16.extractBase16 . B16.encodeBase16 . getEventId $ e

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e -> Event
    <$> e .: "id"
    <*> e .: "pubkey"
    <*> (fromSeconds <$> e .: "created_at")
    <*> e .: "kind"
    <*> e .: "tags"
    <*> e .: "content"
    <*> e .: "sig"

instance ToJSON Event where
  toJSON Event {..} = object
     [ "id"         .= exportEventId eventId
     , "pubkey"     .= exportXOnlyPubKey pubKey
     , "created_at" .= toSeconds created_at
     , "kind"       .= kind
     , "tags"       .= tags
     , "content"    .= content
     , "sig"        .= exportBip340Sig sig
     ]

instance ToJSON UnsignedEvent where
  toJSON (UnsignedEvent {..}) = Array $ fromList
     [ Number 0
     , String $ pack $ exportXOnlyPubKey $ pubKey'
     , Number $ fromIntegral $ toSeconds $ created_at'
     , toJSON kind'
     , toJSON tags'
     , toJSON content'
     ]

instance FromJSON Tag where
 parseJSON (Array v)
   | V.length v > 0 =
       case v V.! 0 of
         String "e" ->
           ETag <$> parseJSON (v V.! 1) <*> parseJSON (fromMaybe Null $ v V.!? 2) <*> parseJSON (fromMaybe Null $ v V.!? 3)
         String "p" ->
           PTag <$> parseJSON (v V.! 1) <*> parseJSON (fromMaybe Null $ v V.!? 2) <*> parseJSON (fromMaybe Null $ v V.!? 3)
         _ ->
           return . UnknownTag $ show v
   | otherwise = return . UnknownTag $ show v
 parseJSON v = return . UnknownTag $ show v

instance ToJSON Tag where
 toJSON (ETag eventId Nothing Nothing) =
   Array $ fromList
     [ String "e"
     , String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId
     ]
 toJSON (ETag eventId relayURL Nothing) =
   Array $ fromList
     [ String "e"
     , String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId
     , maybe (String "") (\r -> String r) relayURL
     ]
 toJSON (ETag eventId relayURL marker) =
   Array $ fromList
     [ String "e"
     , String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId
     , maybe (String "") (\r -> String r) relayURL
     , case marker of
         Just Reply ->
           String "reply"
         Just Root ->
           String "root"
         Just Mention ->
           String "mention"
         Nothing ->
           String ""
     ]
 toJSON (PTag xo relayURL name) =
   Array $ fromList
     [ String "p"
     , case xo of
         ValidXOnlyPubKey xo' ->
           toJSON xo'
         InvalidXOnlyPubKey ->
           String ""
     , maybe (String "") (\r -> String r) relayURL
     , maybe (String "") (\n -> String n) name
     ]
 toJSON _ = -- @todo implement nonce tag
   Array $ fromList []

instance FromJSON Marker where
 parseJSON = withText "Marker" $ \m -> do
   case toLower m of
     "reply" -> return Reply
     "root"  -> return Root
     "mention"  -> return Mention
     _       -> mzero

instance ToJSON Marker where
 toJSON (Reply) = String "reply"
 toJSON (Root) = String "root"
 toJSON (Mention) = String "mention"

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _  -> Nothing

exportEventId :: EventId -> String
exportEventId i = unpack . B16.extractBase16 . B16.encodeBase16 $ getEventId i

signEvent :: UnsignedEvent -> KeyPair -> XOnlyPubKey -> Either Text Event
signEvent u kp xo = flip (maybe (Left "signing failed")) msig $ 
  \signature -> Right Event
      { eventId = eid
      , pubKey = xo
      , created_at = created_at' u
      , kind = kind' u
      , tags = tags' u
      , content = content' u
      , sig = signature
      }
  where
    eid = EventId {getEventId = SHA256.hash $ toStrict $ encode u}
    msig = signBip340 kp $ fromJust $ msg $ getEventId eid

validateEventId :: Event -> Bool
validateEventId e = (getEventId $ eventId e) == (SHA256.hash $ toStrict $ encode e)

verifySignature :: Event -> Bool
verifySignature e =
  case msg $ toStrict $ encode e of
    Just m  -> verifyBip340 p m s
    Nothing -> False
  where
    p = pubKey e
    s = sig e

textNote :: Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = TextNote, tags' = [], content' = note}

setMetadata :: Profile -> XOnlyPubKey -> DateTime -> UnsignedEvent
setMetadata profile xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = Metadata
    , tags' = []
    , content' = LazyText.toStrict . toLazyText . encodeToTextBuilder . toJSON $ profile
    }

readProfile :: Event -> Maybe Profile
readProfile event = case kind event of
  Metadata ->
    decode $ fromStrict $ encodeUtf8 $ content event
  _ ->
    Nothing

replyNote :: Event -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
replyNote event note xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = TextNote, tags' = [ETag (eventId event) Nothing (Just Reply)], content' = note}

setContacts :: [(XOnlyPubKey, Maybe Username)] -> XOnlyPubKey -> DateTime -> UnsignedEvent
setContacts contacts xo t =
  UnsignedEvent
    { pubKey' = xo
    , created_at' = t
    , kind' = Contacts
    , tags' = map (\c -> PTag (ValidXOnlyPubKey $ fst c) (Just "") (snd c)) contacts
    , content' = ""
    }

deleteEvents :: [EventId] -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
deleteEvents eids reason xo t =
  UnsignedEvent
    {pubKey' = xo, created_at' = t, kind' = Delete, tags' = toDelete, content' = reason}
  where
    toDelete = map (\eid -> ETag eid Nothing Nothing) eids

getReplyEventId :: Event -> Maybe EventId
getReplyEventId = getMarkerEventId Reply

getRootEventId :: Event -> Maybe EventId
getRootEventId = getMarkerEventId Root

getMarkerEventId :: Marker -> Event -> Maybe EventId
getMarkerEventId m e =
  if null replyList
    then Nothing
    else Just $ extractEventId $ head replyList
  where
    replyFilter :: Marker -> Tag -> Bool
    replyFilter m (ETag _ _ (Just m')) = m == m'
    replyFilter m _ = False

    replyList = filter (replyFilter m) $ tags e

    extractEventId :: Tag -> EventId
    extractEventId (ETag eid _ _) = eid
    extractEventId _ = error "Could not extract event id from reply or root tag"

getParentId :: ReceivedEvent -> Maybe EventId
getParentId event =
  let eTags = filter isEtag . tags . fst $ event
      replyTag = do
        (ETag eid _ _) <- find isReplyTag eTags
        pure eid
  in
      case replyTag of
        Just _ -> replyTag -- if found reply tag than this is the parent
        Nothing -> do -- else see if you find a root tag
          (ETag eid _ _) <- find isRootTag eTags
          pure eid
  where

    isEtag ETag {} = True
    isEtag _ = False

    isRootTag (ETag _ _ (Just Root)) = True
    isRootTag _ = False


isReplyTag :: Tag -> Bool
isReplyTag (ETag _ _ (Just Reply)) = True
isReplyTag _ = False

isReply :: Event -> Bool
isReply = any isReplyTag . tags

isReplyTo :: Event -> Event -> Bool
event `isReplyTo` parent = any checkTag . tags $ event
  where
    checkTag (ETag eid _ (Just Reply)) = eid == eventId parent
    checkTag _ = False