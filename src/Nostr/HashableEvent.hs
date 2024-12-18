{-# LANGUAGE OverloadedStrings #-}

-- Deserialize the event in such a way that it's easy to compute it's hash correctly
module Nostr.HashableEvent where

import Data.Aeson
import GHC.IsList (fromList)

-- Just pick the fields from event which go into the hash without deserializing them
data HashableEvent = HashableEvent
  { pubKey :: Value,
    created_at :: Value,
    kind :: Value,
    tags :: Value,
    content :: Value
  } deriving (Eq, Show)

instance FromJSON HashableEvent where
  parseJSON = withObject "event data" $ \e ->
    HashableEvent
      <$> e .: "pubkey"
      <*> e .: "created_at"
      <*> e .: "kind"
      <*> e .: "tags"
      <*> e .: "content"

instance ToJSON HashableEvent where 
  toJSON se = 
     Array $
      fromList
        [ Number 0,
          pubKey se,
          created_at se,
          kind se,
          tags se,
          content se
        ]