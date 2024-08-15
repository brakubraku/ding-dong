{-# LANGUAGE CPP                        #-}
{-# LANGUAGE OverloadedStrings          #-}
-- {-# LANGUAGE QuasiQuotes       #-}

module Nostr.JSM where 

import           Miso.FFI
import Miso.FFI.WebSocket as WS
import Miso.Effect
import Miso.Subscription.WebSocket
import Data.Aeson
import           GHCJS.Marshal
import           GHCJS.Foreign
import           GHCJS.Types ()
import Control.Monad.IO.Class
import Miso.String
import Nostr.Relay 
import qualified Text.URI.QQ as QQ
import Text.URI (mkURI, emptyURI)

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
    go n    = OtherCode n

secs :: Int -> Int
secs = (*1000000)

sendJson' :: ToJSON json => Socket -> json -> JSM ()
sendJson' socket m = WS.send socket =<< stringify m

send :: ToJSON a => Socket -> a -> JSM ()
{-# INLINE send #-}
send s x = do
  sendJson' s x

startWebSocketSubscriptions
  :: FromJSON m
  => (WebSocket m -> action)
  -> Sub action
startWebSocketSubscriptions f sink = do
--   void . forkJSM $ handleReconnect
  initRelay . Prelude.head $ defaultRelays
  where
    initRelay relay = do
      socket <- createWebSocket ("wss://sg.qemura.xyz") []
      WS.addEventListener socket "open" $ \_ -> liftIO $ do
        sink (f WebSocketOpen)
      WS.addEventListener socket "message" $ \v -> do
        d <- parse =<< WS.data' v
        liftIO . sink $ f (WebSocketMessage d)
      WS.addEventListener socket "close" $ \e -> do
        code <- codeToCloseCode <$> WS.code e
        -- liftIO (writeIORef closedCode (Just code))
        reason <- WS.reason e
        clean <- WS.wasClean e
        liftIO . sink $ f (WebSocketClose code clean reason)
      WS.addEventListener socket "error" $ \v -> do
        -- liftIO (writeIORef closedCode Nothing)
        d' <- WS.data' v
#ifndef ghcjs_HOST_OS
        undef <- ghcjsPure (isUndefined d')
#else
        let undef = isUndefined d'
#endif
        if undef
          then do
            liftIO . sink $ f (WebSocketError mempty)
          else do
            Just d <- fromJSVal d'
            liftIO . sink $ f (WebSocketError d)

    -- handleReconnect = do
    --   liftIO (threadDelay (secs 3))
    --   Just s <- liftIO (readIORef websocket)
    --   status <- WS.socketState s
    --   code <- liftIO (readIORef closedCode)
    --   if status == 3
    --     then do
    --       unless (code == Just CLOSE_NORMAL) $
    --         websocketSub (URL u) (Protocols ps) f sink
    --     else handleReconnect


defaultRelays :: [Relay]
defaultRelays =
  [
    Relay
      { uri = emptyURI,
        info = RelayInfo True True,
        connected = False
      }
    -- Relay
    --   { uri = "wss://sg.qemura.xyz",
    --     info = RelayInfo True True,
    --     connected = False
    --   }
    -- Relay
    --   { uri = [QQ.uri|wss://nostr-02.dorafactory.org|],
    --     info = RelayInfo True True,
    --     connected = False
    --   }  ,
    -- Relay
    --   { uri = [QQ.uri|wss://nostr.wine|],
    --     info = RelayInfo True True,
    --     connected = False
    --   }
  ]