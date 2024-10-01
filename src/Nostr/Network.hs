{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
-- optics support
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Network where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad.Reader
import Data.List (partition)
import Data.Map hiding (partition)
import qualified Data.Map as Map hiding (partition)
import Data.Maybe
import Data.Text hiding (partition, zip)
import GHC.Float
import GHC.Generics
import Nostr.Keys
import Nostr.Relay
import Nostr.Request
import Nostr.Response hiding (EOSE)
import Optics

data SubscriptionState = SubscriptionState
  { relaysState :: Map Relay RelaySubState,
    responseCh :: TChan (Response, Relay)
  }
  deriving (Generic, Eq)

printState :: SubscriptionState -> Text
printState ss =
  let printRel (r, s) = r ^. #uri <> ":" <> pack (show s)
   in intercalate "\n" $ printRel <$> Map.toList (ss ^. #relaysState)

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

data RelaySubState = Running | EOSE | Error Text deriving (Eq, Show)

-- Subscription is considered finished when none of Relays is in a Running state
-- for that particular subscription Id.
isSubFinished :: SubscriptionId -> Map SubscriptionId SubscriptionState -> Bool
isSubFinished sid ss =
  fromMaybe True $ do
    subState <- Map.lookup sid ss
    pure . notElem Running . elems $ subState ^. #relaysState

ratioOfFinished :: SubscriptionId -> Map SubscriptionId SubscriptionState -> Float
ratioOfFinished sid ss = fromMaybe 1 $ do
  rs <- Map.elems . view #relaysState <$> Map.lookup sid ss
  let (running, other) = partition (== Running) rs
      (eose, _) = partition (== EOSE) other
      length = toInteger . Prelude.length
      ratio =
        int2Float (fromInteger . length $ eose)
          / int2Float (fromInteger . length $ running ++ eose) -- TODO: wtf
  pure ratio

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
