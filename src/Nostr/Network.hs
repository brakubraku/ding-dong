{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- optics support
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Network where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.Map
import qualified Data.Map as Map
import Data.Maybe
import Data.Text hiding (zip)
import Debug.Trace
import GHC.Generics
import Nostr.Keys
import Nostr.Relay
import Nostr.Request
import Nostr.Response
import Optics

data SubscriptionState = SubscriptionState
  { relaysState :: Map Relay RelaySubState,
    responseCh :: TChan (Response, Relay)
  }
  deriving (Generic, Eq)

data NostrNetwork = NostrNetwork
  { relays :: MVar (Map.Map RelayURI Relay),
    subscriptions :: MVar (Map SubscriptionId SubscriptionState),
    requestCh :: TChan Request,
    keys :: Keys
  }
  deriving (Generic, Eq)

runNostr :: NostrNetwork -> NostrNetworkT a -> IO a
runNostr = flip runReaderT

type NostrNetworkT = ReaderT NostrNetwork IO

data RelaySubState =  Running | EOSE | Error Text deriving (Eq, Show)

-- Subscription is considered finished when none of Relays is in a Running state
-- for that particular subscription Id.
isSubFinished :: SubscriptionId -> Map SubscriptionId SubscriptionState -> Bool
isSubFinished subId subStates =
  fromMaybe False $ do
    -- subState <- Map.lookup subId subStates
    subState <- trace ("branko-subId-isSubFinished:subId=" <> show subId <> " " <> (show . preview (_Just % #relaysState) . Map.lookup subId) subStates) Map.lookup subId subStates
    let states = elems $ subState ^. #relaysState
    pure $ notElem Running states 

initNetwork :: [RelayURI] -> Keys -> IO NostrNetwork
initNetwork relays keys = do
  relays <- newMVar . fromList . zip relays $ (newRelay <$> relays)
  requestCh <- atomically newTChan
  subscriptions <- newMVar Map.empty
  pure NostrNetwork {..}
  where
    newRelay :: RelayURI -> Relay
    newRelay ru =
      Relay
        { uri = ru,
          connected = False,
          info = RelayInfo True True
        }
