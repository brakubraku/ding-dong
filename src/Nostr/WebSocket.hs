{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Nostr.WebSocket
  ( WebSocketAction(..)
  , URL(..)
  , Protocols(..)
  , SocketState(..)
  , CloseCode(..)
  , WasClean(..)
  , Reason(..)
  , WSError(..)
  , connectRelays
  ) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (bracket, try, finally)
import Control.Monad
import Control.Monad.Reader
import Control.Monad.Except (throwError)
import Data.Aeson
import Data.ByteString (fromStrict)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text.Encoding (encodeUtf8)
import GHCJS.Foreign
import GHCJS.Marshal
import GHCJS.Types ()
import Language.Javascript.JSaddle.String
import Language.Javascript.JSaddle.Value (valToStr)
import Miso.Effect (Sub)
import Miso.FFI
import Miso.FFI.WebSocket (Socket)
import qualified Miso.FFI.WebSocket as WS
import Miso.String
import Miso.WebSocket hiding (WebSocket(..))
import Nostr.Log
import Nostr.Network
import Nostr.Relay
import Nostr.RelayPool
import Nostr.Response
import Optics
import Prelude hiding (map)
import Data.Time
import Nostr.Event (verifySignature)
import Utils

data WSError 
  = ConnectionError Text
  | DecodingError Text
  | VerificationError Text
  deriving (Show, Eq)

data WebSocketAction 
  = WebSocketOpen Relay
  | WebSocketClose Relay Text 
  | WebSocketError Relay Text

throwError :: WSError -> Text -> JSM a
throwError err msg = error $ show err <> ": " <> show msg

-- | Safe resource management for event listeners
withEventListener :: Socket -> Text -> (a -> JSM ()) -> JSM () -> JSM ()
withEventListener socket event handler cleanup = 
  bracket 
    (WS.addEventListener socket event handler)
    (\_ -> cleanup) 
    (\_ -> return ())

-- | Safe JSON sending with error handling
sendJson' :: (ToJSON json) => Socket -> json -> JSM ()
sendJson' socket msg = do
  result <- try $ WS.send socket =<< stringify msg
  case result of
    Left err -> logError $ "Failed to send: " <> show err
    Right _ -> return ()

-- | Exponential backoff for reconnection
reconnectWithBackoff :: NostrNetwork -> Int -> UTCTime -> Relay -> JSM ()
reconnectWithBackoff nn times lastAttempt relay = do
  now <- liftIO getCurrentTime
  let delay = min (2^times * 500) 5000  -- Cap at 5 seconds
  liftIO $ threadDelay (delay * 1000)
  connectRelay nn (times + 1, now) relay

-- | Clean up subscription resources
cleanupSubscription :: NostrNetwork -> Text -> IO ()
cleanupSubscription nn subId = do
  subs <- readMVar (nn ^. #subscriptions)
  forM_ (Map.lookup subId subs) $ \sub -> do
    atomically $ closeTChan (sub ^. #responseCh)
    modifyMVar_ (nn ^. #subscriptions) (pure . Map.delete subId)

connectRelays :: NostrNetwork -> (WebSocketAction -> action) -> Sub action
connectRelays nn sendMsg sink = do
  relays <- liftIO $ readMVar (nn ^. #relays)
  now <- liftIO getCurrentTime
  mapM_ (forkJSM . connectRelay nn (0, now)) relays

-- | Main connection logic with proper resource management
connectRelay :: NostrNetwork -> (Int, UTCTime) -> Relay -> JSM ()
connectRelay nn (recnt, lastReconnect) relay = do
  reconnectingRef <- liftIO $ newMVar False
  
  socket <- createWebSocket (relay ^. #uri) []
  requestChan <- liftIO . atomically . dupTChan $ (nn ^. #requestCh)

  -- Atomic reconnection check
  let tryReconnect = modifyMVar reconnectingRef $ \isReconnecting ->
        if isReconnecting 
          then return (isReconnecting, False)
          else do
            markIsConnected nn False relay
            return (True, True)

  -- Event handlers with cleanup
  let setupHandlers = do
        withEventListener socket "open" $ \_ -> 
          liftIO $ do
            markIsConnected nn True relay
            sink . sendMsg $ WebSocketOpen relay

        withEventListener socket "message" $ \v -> do
          msg <- valToStr =<< WS.data' v
          handleMessage nn msg relay

        withEventListener socket "close" $ \e -> do
          code <- codeToCloseCode <$> WS.code e
          reason <- WS.reason e
          clean <- WS.wasClean e
          liftIO $ do
            sink . sendMsg $ WebSocketClose relay (decodeError code clean reason)
            shouldReconnect <- tryReconnect
            when shouldReconnect $ reconnectWithBackoff nn recnt lastReconnect relay

        withEventListener socket "error" $ \v -> do
          d' <- WS.data' v
          errMsg <- if isUndefined d'
            then return mempty
            else fromMaybe mempty <$> fromJSVal d'
          liftIO $ do
            sink . sendMsg $ WebSocketError relay errMsg
            shouldReconnect <- tryReconnect
            when shouldReconnect $ reconnectWithBackoff nn recnt lastReconnect relay

  -- Message loop with proper state handling
  let messageLoop = forever $ do
        state <- WS.socketState socket
        case state of
          0 -> liftIO $ threadDelay 100000  -- Not ready
          1 -> do -- Ready
            reqs <- liftIO . atomically $ tryReadTChan requestChan
            mapM_ (sendJson' socket) reqs
            liftIO $ threadDelay 50000
          _ -> do -- Error states
            liftIO $ markAllError nn relay "Connection error"
            throwError ConnectionError "Socket in error state"

  -- Cleanup on exit
  finally setupHandlers $ do
    liftIO $ do
      atomically $ closeTChan requestChan
      markIsConnected nn False relay

-- | Safe message handling with proper error handling
handleMessage :: NostrNetwork -> Text -> Relay -> JSM ()
handleMessage nn msg relay = do
  case eitherDecode @Response . fromStrict . encodeUtf8 . strToText $ msg of
    Right resp -> handleResponse nn resp relay
    Left err -> logRelayError relay $ "Decode error: " <> pack (show err)

handleResponse :: NostrNetwork -> Response -> Relay -> JSM ()
handleResponse nn resp relay = case resp of
  EventReceived subId event 
    | not (verifySignature event) -> 
        logRelayError relay $ "Invalid signature: " <> pack (show event)
    | otherwise -> do
        subs <- liftIO $ readMVar (nn ^. #subscriptions)
        forM_ (Map.lookup subId subs) $ \sub ->
          liftIO . atomically $ writeTChan 
            (sub ^. #responseCh) 
            (EventReceived subId event, relay)
            
  EOSE subId -> 
    liftIO $ runReaderT (changeState subId relay (fmap . const $ Network.EOSE)) nn
    
  OK eid True _ ->
    liftIO $ runReaderT (setResultSuccess eid relay) nn
    
  OK eid False reason ->
    liftIO $ runReaderT (setResultError (fromMaybe "" reason) eid relay) nn
    
  _ -> logRelayError relay $ "Unknown response: " <> pack (show resp)

markIsConnected :: NostrNetwork -> Bool -> Relay -> IO ()
markIsConnected nn isCon r =  
    modifyMVar_ (nn ^. #relays) $ \rels ->
      pure $ rels & at (r ^. #uri) % _Just % #connected .~ isCon

markAllError :: NostrNetwork -> Relay -> Text -> IO ()
markAllError nn relay eText = do 
  markIsConnected nn False relay 
  runNostr nn $ changeStateForAllSubs relay (fmap . const $ Nostr.Network.Error eText)

createWebSocket :: MisoString -> [MisoString] -> JSM Socket
{-# INLINE createWebSocket #-}
createWebSocket url' protocols =
  WS.create url' =<< toJSVal protocols

codeToCloseCode :: Int -> CloseCode
codeToCloseCode = go
  where
    go 1000 = CLOSE_NORMAL
    go 1001 = CLOSE_GOING_AWAY
    go 1002 = CLOSE_PROTOCOL_ERROR
    go 1003 = CLOSE_UNSUPPORTED
    go 1005 = CLOSE_NO_STATUS
    go 1006 = CLOSE_ABNORMAL
    go 1007 = Unsupported_Data
    go 1008 = Policy_Violation
    go 1009 = CLOSE_TOO_LARGE
    go 1010 = Missing_Extension
    go 1011 = Internal_Error
    go 1012 = Service_Restart
    go 1013 = Try_Again_Later
    go 1015 = TLS_Handshake
    go n = OtherCode n

decodeError :: CloseCode -> WasClean -> Reason -> Text
decodeError _ _ _ = "Connection closed" -- TODO: Implement proper error decoding