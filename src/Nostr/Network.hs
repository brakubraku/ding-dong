{-# LANGUAGE RecordWildCards #-}
-- optics support
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric              #-}
module Nostr.Network where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map
import Nostr.Relay
import Nostr.Keys
import Nostr.Request
import Nostr.Response
import Data.Text hiding (zip)
import qualified Data.Map as Map
import Data.Maybe
import GHC.Generics
import Optics

data SubscriptionState = SubscriptionState {
  relaysState :: Map Relay RelaySubState,
  responseCh :: TChan (Response, Relay)
} deriving (Generic, Eq) 

data NostrNetwork = NostrNetwork
  { relays :: MVar (Map.Map RelayURI Relay),
    subscriptions :: MVar (Map SubscriptionId SubscriptionState),
    requestCh :: TChan Request,
    connEventCh :: TChan ConnectionEvent,
    keys :: Keys
  } deriving (Generic, Eq)
  
runNostr :: NostrNetwork -> NostrNetworkT a -> IO a
runNostr = flip runReaderT

type NostrNetworkT = ReaderT NostrNetwork IO

data RelaySubState = Running | EOSE | Error Text deriving (Eq, Show)
data ConnectionEvent = RelaysUpdatedE [Relay] | SubscriptionStateUpdatedE (Map SubscriptionId (Map Relay RelaySubState))
  deriving (Show)

-- Subscription is considered finished when none of Relays is in a Running state 
-- for that particular subscription Id.
isSubFinished :: SubscriptionId -> Map SubscriptionId SubscriptionState -> Bool
isSubFinished subId subStates =
  fromMaybe False $ do
    subState <- Map.lookup subId subStates
    pure . notElem Running . elems $ subState ^. #relaysState

initNetwork :: [RelayURI] -> IO NostrNetwork
initNetwork relays = do
  relays <- newMVar . fromList . zip relays $ (newRelay <$> relays)
  -- requestCh <- atomically $ do 
  --   ch <- newBroadcastTChan
  --   dupTChan ch
  requestCh <- atomically newTChan
  connEventCh <- atomically newTChan
  subscriptions <- newMVar Map.empty
  pure NostrNetwork {..}
 
 where 

  newRelay :: RelayURI -> Relay 
  newRelay ru = 
    Relay {
      uri = ru,
      connected = False,
      info = RelayInfo True True
    }

allConnected :: NostrNetwork -> IO Bool
allConnected NostrNetwork {..} = do
  rcs <- Map.elems <$> readMVar relays
  pure . Prelude.all connected $ rcs
  -- pure . all snd $ (elems rcs)

