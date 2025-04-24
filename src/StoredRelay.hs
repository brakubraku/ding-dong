{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module StoredRelay where 
import Nostr.Relay
import Miso
-- import Miso.Storage

import Nostr.Network (newRelay)
import Data.Aeson
import GHC.Generics

data StoredRelay = StoredRelay {
    relay :: Relay, 
    active :: Bool
} deriving (FromJSON, ToJSON, Generic, Eq, Show)

loadRelays :: JSM [StoredRelay]
loadRelays = do
  let id = "my-relays"
  rels <- getLocalStorage id
  case rels of
    Right k -> pure k
    Left _ -> do
      saveRelays defaultRelays
      pure defaultRelays

saveRelays :: [StoredRelay] -> JSM ()
saveRelays rels = setLocalStorage "my-relays" rels

newActiveRelay :: Relay -> StoredRelay
newActiveRelay r = StoredRelay r True

defaultRelays :: [StoredRelay]
defaultRelays =
  let active = newActiveRelay . newRelay
        <$> [ "wss://relay.primal.net",
              "wss://relay.nostr.band",
              "wss://relay.damus.io",
              "wss://nos.lol"
            ] 
      inactive = flip StoredRelay False . newRelay <$> 
            [ "wss://relay.nostr.net",
              "wss://polnostr.xyz",
              "wss://nostr.wine",
              "wss://nostr.at"
            ] 
  in 
    active ++ inactive