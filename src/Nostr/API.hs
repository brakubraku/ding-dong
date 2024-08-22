{-# LANGUAGE TemplateHaskell #-}

module Nostr.API where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Data.Map
import Nostr.Relay
import Nostr.Request
import Nostr.Event (ReceivedEvent)
import Nostr.Filter
import Data.Aeson
import Data.Aeson.TH
import Data.Char ( toLower )
import Nostr.Network

-- This module was introduced to decouple Nostr related functionality
-- from the Monomer frontend

informRelaysUpdated :: TChan ConnectionEvent -> [Relay] -> IO ()
informRelaysUpdated chan relays = atomically . writeTChan chan $ RelaysUpdatedE relays

informSubStateUpdated :: TChan ConnectionEvent -> Map SubscriptionId (Map Relay RelaySubState) -> IO ()
informSubStateUpdated chan state = do
  atomically . writeTChan chan . SubscriptionStateUpdatedE $ state

-- data ClientRequest = Subscribe DatedFilter deriving Eq

-- data ServerResponse = Subcription SubscriptionId [ReceivedEvent]

-- $(deriveJSON defaultOptions{constructorTagModifier = fmap toLower} ''ClientRequest)