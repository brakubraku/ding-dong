{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module StoredRelay where 
import Nostr.Relay
import Miso (JSM)
import Miso.Effect.Storage
import Nostr.Network (newRelay)
import Data.Aeson
import GHC.Generics

data StoredRelay = StoredRelay {
    relay :: Relay, 
    active :: Bool
} deriving (FromJSON, ToJSON, Generic, Eq)

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
  newActiveRelay . newRelay
    <$> [ "wss://relay.nostrdice.com",
          "wss://lunchbox.sandwich.farm",
          "wss://relay.nostr.net",
          "wss://polnostr.xyz",
          "wss://relay.damus.io",
          "wss://nostr.at"
        ]