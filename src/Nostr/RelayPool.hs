{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE FlexibleContexts     #-}

module Nostr.RelayPool where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import System.Entropy
import Data.Aeson (encode)
import qualified Data.ByteString.Base16 as B16
import qualified Data.Base16.Types as B16
import qualified Data.ByteString.Lazy as LazyBytes
import Data.List (sort)
import qualified Data.Map as Map
import Data.Text (pack)
import Debug.Trace ()
import Nostr.Log
import Nostr.API (informSubStateUpdated)
import Nostr.Filter
import Nostr.Network
import Nostr.Relay
import Nostr.Request hiding (subId)
import Nostr.Response
-- import Nostr.Utils
import Optics

changeStateForAllSubs :: Relay -> RelaySubState -> NostrNetworkT ()
changeStateForAllSubs relay state = do
  network <- ask
  updated <- lift . modifyMVar (network ^. #subscriptions) $ \subs -> do
    let updatedSubs = Map.map updateRelaysState subs
    pure (updatedSubs, updatedSubs)
  lift . informSubStateUpdated (network ^. #connEventCh) $ Map.map (^. #relaysState) updated
  where
    updateRelaysState subState = subState & #relaysState %~ Map.insert relay state
    -- updateRelaysState subState = subState & relaysState .~ Map.insert relay state (subState ^. relaysState)
    
changeState :: SubscriptionId -> Relay -> RelaySubState -> NostrNetworkT ()
changeState subId relay state = do
  network <- ask
  lift . modifyMVar_ (network ^. #subscriptions) $ \subs -> do
    case Map.lookup subId subs of
      Nothing -> pure subs
      -- let updated = Map.insert subId (Map.insert relay state empty) subs
      -- lift . putMVar (network ^. subscriptions) $ updated
      Just subState -> do
        let updated = subState & #relaysState %~ Map.insert relay state
            -- let updated = Map.insert relay state relaySubState
            updatedSubs = Map.insert subId updated subs
        informSubStateUpdated (network ^. #connEventCh) . Map.map (^. #relaysState) $ updatedSubs
        pure updatedSubs

addRelay :: Relay -> NostrNetworkT [Relay]
addRelay relay = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = rels & at (relay ^. #uri) .~ Just relay
    pure (updated, Map.elems updated)
  -- NostrNetwork relaysM _ _ _ <- ask
  -- relays <- lift . takeMVar $ relaysM
  -- let relays' = sort $ relay : Prelude.filter (\r -> not $ r `sameRelay` relay) relays
  -- lift . putMVar relaysM $ relays'
  -- lift . saveRelays $ relays'
  -- return relays'

removeRelay :: Relay -> NostrNetworkT [Relay]
removeRelay relay = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = Map.delete (relay ^. #uri) rels
    pure (updated, Map.elems updated)
  -- NostrNetwork relaysM _ _ _ <- ask
  -- relays <- lift . takeMVar $ relaysM
  -- let relays' = Prelude.filter (\r -> not $ r `sameRelay` relay) relays
  -- lift . putMVar relaysM $ relays'
  -- lift . saveRelays $ relays'
  -- return relays'

saveRelays :: [Relay] -> IO ()
saveRelays relays = do
  LazyBytes.writeFile "relays.ft" $ encode relays
  putStrLn "Relays saved to disk"

subscribe :: TChan (Response, Relay) -> [DatedFilter] -> NostrNetworkT SubscriptionId
subscribe responseChannel filters = do
  subId <- lift generateSubId
  registerResponseChannel subId responseChannel
  lift . logDebug $ "Subscribing for filters: " <> (pack . show) filters
  send . Subscribe . Subscription filters $ subId
  return subId
  where
    generateSubId = B16.extractBase16 . B16.encodeBase16 <$> getEntropy 6

    registerResponseChannel :: SubscriptionId -> TChan (Response, Relay) -> NostrNetworkT ()
    registerResponseChannel subId responseChannel = do
      network <- ask
      -- set subscription state to Running for all relays
      rels <- Map.elems <$> (liftIO . readMVar) (network ^. #relays)
      let subsRunning = Map.fromList . zip rels $ (repeat Running)
      lift . modifyMVar_ (network ^. #subscriptions) $
        pure . Map.insert subId (SubscriptionState subsRunning responseChannel)
    

subscribeForFilter ::
  [DatedFilter] ->
  -- return readonly response channel
  NostrNetworkT (TChan (Response, Relay), SubscriptionId)
subscribeForFilter fs = do
  -- lift $ do
  responseCh <- lift . atomically $ newTChan
  subId <- subscribe responseCh fs
  pure (responseCh, subId)

unsubscribe :: SubscriptionId -> NostrNetworkT ()
unsubscribe subId = do
  network <- ask
  send $ Close subId
  lift . modifyMVar_ (network ^. #subscriptions) $
    \subs -> pure $ Map.delete subId subs

send :: Request -> NostrNetworkT ()
send request@(Subscribe sub)
  | isUnbounded sub = do
    lift . logWarning $ "Unbounded subscription:" <> show sub
    send' request
  | otherwise =
    send' request
  where
    isUnbounded sub = not (any isAnytime (filters sub))
    logWarning text = putStrLn $ "Warning: " <> text
send request = send' request

send' :: Request -> NostrNetworkT ()
send' request = do 
  network <- ask 
  lift . atomically . writeTChan (network ^. #requestCh) $ request

waitForActiveConnections :: Int -> NostrNetworkT ()
waitForActiveConnections timeout = do
  network <- ask
  relays' <- lift . readMVar $ (network ^. #relays)
  if all connected relays' || timeout <= 0
    then return ()
    else do
      lift . threadDelay $ 100000
      waitForActiveConnections (timeout - 100000)
