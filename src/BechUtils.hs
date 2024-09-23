{-# LANGUAGE OverloadedStrings #-}

module BechUtils where

import Codec.Binary.Bech32
import Data.Bool (bool)
import qualified Data.ByteString as BS
import Data.Either.Extra
import Data.Maybe
import Data.Text
import Data.Word
import MyCrypto
import Nostr.Event

data Bech = NPub XOnlyPubKey | NEvent EventId
  deriving (Show, Eq)

isNPub :: Bech -> Bool
isNPub (NPub _) = True
isNPub (NEvent _) = False

partitionBechs :: [Bech] -> ([EventId], [XOnlyPubKey])
partitionBechs bechs =
  let part b (eids, xos) =
        case b of
          NEvent eid -> (eid : eids, xos)
          NPub xo -> (eids, xo : xos)
   in Prelude.foldr part ([], []) bechs

extractEventIds :: [Bech] -> [EventId]
extractEventIds bs =
  catMaybes $
    ( \b -> case b of
        NEvent eid -> Just eid
        _ -> Nothing
    )
      <$> bs

extractProfileIds :: [Bech] -> [XOnlyPubKey]
extractProfileIds bs =
  catMaybes $
    ( \b -> case b of
        NPub pid -> Just pid
        _ -> Nothing
    )
      <$> bs

parseTLVs :: BS.ByteString -> [(Int, BS.ByteString)]
parseTLVs bs =
  let tlv bs = do
        (t, lvrest) <- BS.uncons bs
        (l, vrest) <- BS.uncons lvrest
        let (v, rest) = BS.splitAt (fromInteger . toInteger $ l) vrest
        pure ((fromInteger . toInteger $ t, v), rest)
      getAll (Just ((t, v), rest)) = (t, v) : getAll (tlv rest)
      getAll Nothing = []
   in getAll (tlv bs)

get :: Int -> [(Int, BS.ByteString)] -> Maybe BS.ByteString
get i ((j, bs) : tlvs) = bool (get i tlvs) (Just bs) $ i == j
get _ [] = Nothing

decodeBech :: Text -> Maybe Bech
decodeBech t = do
  (hp, dp) <- eitherToMaybe $ decodeLenient t -- TODO: decodeLenient has worse performance
-- >>>  decodeLenient "nevent1qqsr0p59c86ll9mh0p6z3fjamyupj626gek2nkfkaxlu5lzyccqkeyspzdmhxue69uhhwmm59e6hg7r09ehkuef0qgstsw3gkljwt5stm9svt7htvcjlj4ffze4chkcyt4pxxj30xkgeg5qrqsqqqqqpvmz4a5"
-- Right (HumanReadablePart "nevent",DataPart "qqsr0p59c86ll9mh0p6z3fjamyupj626gek2nkfkaxlu5lzyccqkeyspzdmhxue69uhhwmm59e6hg7r09ehkuef0qgstsw3gkljwt5stm9svt7htvcjlj4ffze4chkcyt4pxxj30xkgeg5qrqsqqqqqp")
  case humanReadablePartToText hp of
    "npub" -> NPub <$> (parseXOnlyPubKey =<< dataPartToBytes dp)
    "nprofile" -> NPub <$> (parseXOnlyPubKey =<< get 0 =<< parseTLVs <$> dataPartToBytes dp)
    "note" -> NEvent <$> (EventId <$> dataPartToBytes dp)
    "nevent" -> NEvent . EventId <$> (get 0 =<< parseTLVs <$> dataPartToBytes dp)
    _ -> Nothing


decodeNpub :: Text -> Maybe XOnlyPubKey
decodeNpub bech = case decodeBech bech of
  Just (NPub xo) -> Just xo
  _ -> Nothing

encodeBechXo :: XOnlyPubKey -> Maybe Text
encodeBechXo xo = do
  prefix <- either (const Nothing) Just $ humanReadablePartFromText "npub"
  let dataPart = dataPartFromBytes . exportToByteString $ xo
  either (const Nothing) Just $ encode prefix dataPart

encodeBechEvent :: EventId -> Maybe Text
encodeBechEvent eid = do
  prefix <- either (const Nothing) Just $ humanReadablePartFromText "nevent"
  let dataPart = dataPartFromBytes . getEventId $ eid
  either (const Nothing) Just $ encode prefix dataPart
