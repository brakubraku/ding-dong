module BechUtils where

import Codec.Binary.Bech32
import Data.Bifunctor
import Data.Text
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


