{-# LANGUAGE OverloadedStrings #-}

module BechUtils where

import Codec.Binary.Bech32
import Data.Bifunctor
import Data.Either (fromRight)
import Data.Text
import qualified Data.Text as T
import Data.Text.Encoding
import MyCrypto
import Nostr.Event

decodeNpub :: Text -> Either () XOnlyPubKey
decodeNpub npub = do
  (_, dp) <- first (const ()) $ decode npub
  maybe (Left ()) pure $ parseXOnlyPubKey =<< dataPartToBytes dp

decodeNote :: Text -> Either () EventId
decodeNote note = do
  (_, dp) <- first (const ()) $ decode note
  maybe (Left ()) pure $ EventId <$> dataPartToBytes dp

bechNpub :: XOnlyPubKey -> Maybe Text
bechNpub xo = do
  prefix <- either (const Nothing) Just $ humanReadablePartFromText "npub"
  let dataPart = dataPartFromBytes . exportToByteString $ xo
  either (const Nothing) Just $ encode prefix dataPart