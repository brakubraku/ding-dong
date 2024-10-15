{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Nostr.RelayPool where

import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar
import Control.Concurrent.STM.TChan
import Control.Monad (unless)
import Control.Monad.Extra (when)
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
import Nostr.Request 
import Nostr.Response
import Optics
import System.Entropy
import Nostr.Event
import Data.DateTime

changeStateForAllSubs ::
  Relay ->
  (Maybe RelaySubState -> Maybe RelaySubState) ->
  NostrNetworkT ()
changeStateForAllSubs r change = do
  nn <- ask
  allSubs <- Map.keys <$> (liftIO . readMVar) (nn ^. #subscriptions)
  mapM_ (\sid -> changeState sid r change) allSubs

changeState ::
  SubscriptionId ->
  Relay ->
  (Maybe RelaySubState -> Maybe RelaySubState) ->
  NostrNetworkT ()
changeState sid relay change = do
  nn <- ask
  lift . modifyMVar_ (nn ^. #subscriptions) $ \subs -> do
    let updated =
          subs
            & at sid
            % _Just
            % #relaysState
            % at relay
            %~ change
    pure updated

addRelay :: Relay -> NostrNetworkT [Relay]
addRelay r = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = rels & at (r ^. #uri) .~ Just r
    pure (updated, Map.elems updated)

removeRelay :: Relay -> NostrNetworkT [Relay]
removeRelay relay = do
  nn <- ask
  lift . modifyMVar (nn ^. #relays) $ \rels -> do
    let updated = Map.delete (relay ^. #uri) rels
    pure (updated, Map.elems updated)

subscribe ::
  TChan (Response, Relay) ->
  [DatedFilter] ->
  NostrNetworkT SubscriptionId
subscribe rch filters = do
  sid <- lift generate
  registerResponseChannel sid rch
  lift . logDebug $ "Subscribing for filters: " <> (pack . show) filters
  send . Subscribe . Subscription filters $ sid
  pure sid
  where
    generate =
      B16.extractBase16
        . B16.encodeBase16
        <$> getEntropy 6

    registerResponseChannel ::
      SubscriptionId ->
      TChan (Response, Relay) ->
      NostrNetworkT ()
    registerResponseChannel sid rch = do
      network <- ask
      -- set subscription state to Running for all relays
      rels <-
        filter (\r -> r ^. #connected)
          . Map.elems
          <$> (liftIO . readMVar) (network ^. #relays)
      let subsRunning = Map.fromList . zip rels $ (repeat Running)
      lift . modifyMVar_ (network ^. #subscriptions) $
        pure . Map.insert sid (SubscriptionState subsRunning rch)

subscribeForFilter ::
  [DatedFilter] ->
  -- TODO: return readonly response channel
  NostrNetworkT (TChan (Response, Relay), SubscriptionId)
subscribeForFilter fs = do
  rch <- lift . atomically $ newTChan
  sid <- subscribe rch fs
  pure (rch, sid)

unsubscribe :: SubscriptionId -> NostrNetworkT ()
unsubscribe sid = do
  nn <- ask
  send $ Close sid
  lift . modifyMVar_ (nn ^. #subscriptions) $
    \subs -> pure $ Map.delete sid subs

send :: Request -> NostrNetworkT ()
send request@(Subscribe sub) = do
  when isUnbounded $
    lift . putStrLn $
      "Warning: " <> "Unbounded subscription:" <> show sub
  send' request
  where
    isUnbounded = not (any isAnytime (filters sub))
send request = send' request

send' :: Request -> NostrNetworkT ()
send' request = do
  nn <- ask
  lift . atomically . writeTChan (nn ^. #requestCh) $ request

sendEvent :: Event -> NostrNetworkT ()
sendEvent e = do
  nn <- ask 
  now <- liftIO getCurrentTime
  rels <- Map.elems <$> liftIO (readMVar $ nn ^. #relays)
  -- when sending an event, set request results as unknown for all connected relays
  liftIO $ modifyMVar_ (nn ^. #requestResults) $ 
    \rr -> do
      let cr = filter (view #connected) rels 
      pure $ rr & at (e ^. #eventId) ?~ (Map.fromList (zip cr (repeat ResultUnknown)), now)
  send . SendEvent $ e

waitForActiveConnections :: Int -> NostrNetworkT ()
waitForActiveConnections timeout = do
  nn <- ask
  rels <- lift . readMVar $ (nn ^. #relays)
  unless (all connected rels || timeout <= 0) $ do
    lift . sleep . Seconds $ 0.5
    waitForActiveConnections (timeout - 100000)

saveRelays :: [Relay] -> IO ()
saveRelays rels = do
  LazyBytes.writeFile "relays.ft" $ encode rels
  putStrLn "Relays saved to disk"