{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.RelayPool where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan

import Control.Monad (unless)
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Data.Aeson (encode)
import qualified Data.Base16.Types as B16
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LazyBytes
import qualified Data.Map as Map
import Data.Text (pack)
import Nostr.Filter
import Nostr.Log
import Nostr.Network
import Nostr.Relay
import Nostr.Request hiding (subId)
import Nostr.Response
import Optics
import System.Entropy

-- TODO: implement this using changeState function
changeStateForAllSubs :: Relay -> (Maybe RelaySubState -> Maybe RelaySubState) -> NostrNetworkT ()
changeStateForAllSubs relay change = do
  network <- ask
  allSubs <- Map.keys <$> (liftIO . readMVar) (network ^. #subscriptions)
  mapM_ (\sid -> changeState sid relay change) allSubs


changeState :: SubscriptionId -> Relay -> (Maybe RelaySubState -> Maybe RelaySubState) -> NostrNetworkT ()
changeState subId relay change = do
  network <- ask
  lift . modifyMVar_ (network ^. #subscriptions) $ \subs -> do
    let updated =
          subs
            & at subId
            % _Just
            % #relaysState
            % at relay
            %~ change
    pure updated

addRelay :: Relay -> NostrNetworkT [Relay]
addRelay relay = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = rels & at (relay ^. #uri) .~ Just relay
    pure (updated, Map.elems updated)

removeRelay :: Relay -> NostrNetworkT [Relay]
removeRelay relay = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = Map.delete (relay ^. #uri) rels
    pure (updated, Map.elems updated)

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
      rels <- filter (\r -> r ^. #connected) .  Map.elems <$> (liftIO . readMVar) (network ^. #relays)
      let subsRunning = Map.fromList . zip rels $ (repeat Running)
      lift . modifyMVar_ (network ^. #subscriptions) $
        pure . Map.insert subId (SubscriptionState subsRunning responseChannel)

subscribeForFilter ::
  [DatedFilter] ->
  -- TODO: return readonly response channel
  NostrNetworkT (TChan (Response, Relay), SubscriptionId)
subscribeForFilter fs = do
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
  unless (all connected relays' || timeout <= 0) $ do
    lift . threadDelay $ 100000
    waitForActiveConnections (timeout - 100000)
