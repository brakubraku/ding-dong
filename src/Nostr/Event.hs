{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Nostr.Event where

import Control.Monad (mzero)
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Text (encodeToTextBuilder)
import qualified Data.Base16.Types as B16
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.DateTime
import Data.Default
import Data.List
import Data.Maybe (catMaybes, fromJust, fromMaybe)
import Data.Text (Text, pack, toLower, unpack)
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.Lazy as LazyText
import Data.Text.Lazy.Builder (toLazyText)
import Data.Time
import qualified Data.Vector as V
import Debug.Trace
import GHC.Exts (fromList)
import GHC.Generics
import MyCrypto
import Nostr.Keys
import Nostr.Kind
import Nostr.Profile (Profile (..), RelayURL, Username)
import Nostr.Relay
import Optics hiding (uncons)
import System.Entropy

newtype EventId = EventId
  { getEventId :: ByteString
  }
  deriving (Eq, Ord)

data Marker = Reply | Root | Mention
  deriving (Eq, Show, Ord)

data Tag
  = ETag EventId (Maybe RelayURL) (Maybe Marker)
  | PTag UnknownXOnlyPubKey (Maybe RelayURL) (Maybe ProfileName)
  | NonceTag
  | UnknownTag String
  deriving (Eq, Show, Ord)

data Event = Event
  { eventId :: EventId,
    pubKey :: XOnlyPubKey,
    created_at :: DateTime,
    kind :: Kind,
    tags :: [Tag],
    content :: Text,
    sig :: Bip340Sig
  }
  deriving (Eq, Show, Generic)

instance Ord Event where
  e1 `compare` e2 = eventId e1 `compare` eventId e2

data UnsignedEvent = UnsignedEvent
  { pubKey' :: XOnlyPubKey,
    created_at' :: DateTime,
    kind' :: Kind,
    tags' :: [Tag],
    content' :: Text
  }
  deriving (Eq, Show)

type ReceivedEvent = (Event, [Relay])

instance Show EventId where
  showsPrec _ = shows . B16.encodeBase16 . getEventId

instance FromJSON EventId where
  parseJSON = withText "EventId" $ \i -> do
    case eventId' i of
      Just e -> return e
      _ -> fail "invalid event id"

-- TODO: check this extractBase is correct
instance ToJSON EventId where
  -- toJSON e = String $ B16.encodeBase16 $ getEventId e
  toJSON e = String . B16.extractBase16 . B16.encodeBase16 . getEventId $ e

instance FromJSON Event where
  parseJSON = withObject "event data" $ \e ->
    Event
      <$> e .: "id"
      <*> e .: "pubkey"
      <*> (fromSeconds <$> e .: "created_at")
      <*> e .: "kind"
      <*> e .: "tags"
      <*> e .: "content"
      <*> e .: "sig"

instance ToJSON Event where
  toJSON Event {..} =
    object
      [ "id" .= exportEventId eventId,
        "pubkey" .= exportXOnlyPubKey pubKey,
        "created_at" .= toSeconds created_at,
        "kind" .= kind,
        "tags" .= tags,
        "content" .= content,
        "sig" .= exportSignature sig
      ]

instance ToJSON UnsignedEvent where
  toJSON (UnsignedEvent {..}) =
    Array $
      fromList
        [ Number 0,
          String $ pack $ exportXOnlyPubKey $ pubKey',
          Number $ fromIntegral $ toSeconds $ created_at',
          toJSON kind',
          toJSON tags',
          toJSON content'
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
    Array $
      fromList
        [ String "e",
          String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId
        ]
  toJSON (ETag eventId relayURL Nothing) =
    Array $
      fromList
        [ String "e",
          String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId,
          maybe (String "") (\r -> String r) relayURL
        ]
  toJSON (ETag eventId relayURL marker) =
    Array $
      fromList
        [ String "e",
          String . B16.extractBase16 . B16.encodeBase16 . getEventId $ eventId,
          maybe (String "") (\r -> String r) relayURL,
          case marker of
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
    Array $
      fromList
        [ String "p",
          case xo of
            ValidXOnlyPubKey xo' ->
              toJSON xo'
            InvalidXOnlyPubKey ->
              String "",
          maybe (String "") (\r -> String r) relayURL,
          maybe (String "") (\n -> String n) name
        ]
  toJSON _ =
    -- @todo implement nonce tag
    Array $ fromList []

instance FromJSON Marker where
  parseJSON = withText "Marker" $ \m -> do
    case toLower m of
      "reply" -> return Reply
      "root" -> return Root
      "mention" -> return Mention
      _ -> mzero

instance ToJSON Marker where
  toJSON (Reply) = String "reply"
  toJSON (Root) = String "root"
  toJSON (Mention) = String "mention"

eventId' :: Text -> Maybe EventId
eventId' t = do
  bs <- decodeHex t
  case BS.length bs of
    32 -> Just $ EventId bs
    _ -> Nothing

exportEventId :: EventId -> String
exportEventId i = unpack . B16.extractBase16 . B16.encodeBase16 $ getEventId i

signEvent :: UnsignedEvent -> SecKey -> XOnlyPubKey -> Maybe Event
signEvent u sk xo = do
  signature <- signBip340 sk . fromJust . msg . getEventId $ eid
  pure
    Event
      { eventId = eid,
        pubKey = xo,
        created_at = created_at' u,
        kind = kind' u,
        tags = tags' u,
        content = content' u,
        sig = signature
      }
  where
    eid = EventId {getEventId = SHA256.hash $ toStrict $ encode u}

validateEventId :: Event -> Bool
validateEventId e = (getEventId $ eventId e) == (SHA256.hash $ toStrict $ encode e)

-- TODO: use this to debug the problems with verifying signatures
-- checking below event (taken from nostr) works with verifyThis function
-- {
--   "id": "a3c9b0bc3c13cb12343917bb4b02b869469f0a8fe3e2e67bd5759373dafa3265",
--   "pubkey": "180a6d42c7d64f8c3958d9d10dd5a4117eaaacea8e7f980781e9a53136cf5693",
--   "created_at": 1723898165,
--   "kind": 1,
--   "tags": [],
--   "content": "nostr pillowtalk, literally ",
--   "sig": "256d4749f68313f902cea8e766f981feac9403b8dffe9b1d04866371a13701b5385a1025e9427b04689a0976075862e7d6b74f0b15ecd48bbbc8733e3908be55"
-- }

verifyThis :: Text -> Text -> Text -> Maybe Bool
verifyThis eid pubKey signature = do
  id <- decodeHex eid
  pk <- decodeHex pubKey >>= parseXOnlyPubKey
  si <- decodeHex signature >>= parseSignature
  mm <- msg id
  pure $ verifyBip340 pk mm si

verifySignature :: Event -> Bool
verifySignature e =
  case msg $ getEventId $ eventId e of
    Just m -> verifyBip340 (pubKey e) m (sig e)
    Nothing -> False

textNote :: Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
textNote note xo t =
  UnsignedEvent
    { pubKey' = xo,
      created_at' = t,
      kind' = TextNote,
      tags' = [],
      content' = note
    }

setMetadata :: Profile -> XOnlyPubKey -> DateTime -> UnsignedEvent
setMetadata profile xo t =
  UnsignedEvent
    { pubKey' = xo,
      created_at' = t,
      kind' = Metadata,
      tags' = [],
      content' = LazyText.toStrict . toLazyText . encodeToTextBuilder . toJSON $ profile
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
    { pubKey' = xo,
      created_at' = t,
      kind' = TextNote,
      tags' = [ETag (eventId event) Nothing (Just Reply)],
      content' = note
    }

setContacts :: [(XOnlyPubKey, Maybe Username)] -> XOnlyPubKey -> DateTime -> UnsignedEvent
setContacts contacts xo t =
  UnsignedEvent
    { pubKey' = xo,
      created_at' = t,
      kind' = Contacts,
      tags' = map (\c -> PTag (ValidXOnlyPubKey $ fst c) (Just "") (snd c)) contacts,
      content' = ""
    }

deleteEvents :: [EventId] -> Text -> XOnlyPubKey -> DateTime -> UnsignedEvent
deleteEvents eids reason xo t =
  UnsignedEvent
    { pubKey' = xo,
      created_at' = t,
      kind' = Delete,
      tags' = toDelete,
      content' = reason
    }
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
   in case replyTag of
        Just _ -> replyTag -- if found reply tag than this is the parent
        Nothing -> do
          -- else see if you find a root tag
          (ETag eid _ _) <- find isRootTag eTags
          pure eid
  where
    isRootTag (ETag _ _ (Just Root)) = True
    isRootTag _ = False

isReplyTag :: Tag -> Bool
isReplyTag (ETag _ _ (Just Reply)) = True
isReplyTag _ = False

isRootTag :: Tag -> Bool
isRootTag (ETag _ _ (Just Root)) = True
isRootTag _ = False

isReply :: Event -> Bool
isReply = any (\t -> isReplyTag t || isRootTag t) . tags

isReplyTo :: Event -> Event -> Bool
event `isReplyTo` parent = any checkTag . tags $ event
  where
    checkTag (ETag eid _ (Just Reply)) = eid == eventId parent
    checkTag _ = False

-- If event has Etag with Reply marker then choose that
-- otherwise if it has Etag with Root marker then choose that
-- otherwise the event is not a response to anything
findIsReplyTo :: Event -> Maybe EventId
findIsReplyTo event =
  let find [] (replyEid, rootEid) = (replyEid, rootEid)
      find _ (replyEid@(Just _), rootEid@(Just _)) = (replyEid, rootEid)
      find (e : tags) (replyEid, rootEid) =
        case e of
          ETag eid _ (Just Reply) -> find tags (Just eid, rootEid)
          ETag eid _ (Just Root) -> find tags (replyEid, Just eid)
          _ -> find tags (replyEid, rootEid)
   in case find (tags event) (Nothing, Nothing) of
        (replyEid@(Just _), _) -> replyEid
        (Nothing, rootEid@(Just _)) -> rootEid
        _ -> Nothing

isEtag :: Tag -> Bool
isEtag ETag {} = True
isEtag _ = False

findETags :: Event -> [Tag]
findETags e = filter isEtag . tags $ e

getSingleETag :: Event -> Maybe Tag
getSingleETag e =
  let etags = findETags e
   in case length etags of
        1 -> fst <$> Data.List.uncons etags
        _ -> Nothing

findRootEid :: Event -> Maybe EventId
findRootEid e = fst <$> (uncons . catMaybes $ findRoot <$> tags e)
  where
    findRoot = \tag -> case tag of
      ETag eid _ (Just Root) -> Just eid
      _ -> Nothing

orderByAgeAsc :: [Event] -> [Event]
orderByAgeAsc es = reverse $
  sortBy
    ( \e1 e2 ->
        (e1 ^. #created_at) `compare` (e2 ^. #created_at)
    )
    es
