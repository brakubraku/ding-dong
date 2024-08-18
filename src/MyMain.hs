{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE CPP                        #-}
-- optics support
{-# LANGUAGE OverloadedLabels     #-}
{-# LANGUAGE DuplicateRecordFields     #-}
{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DeriveGeneric              #-}

module MyMain where

import           Data.Aeson
import           GHC.Generics
import           Data.Bool
import qualified Data.Map as M

import           Miso hiding (send)
import           Miso.String  (MisoString)
import qualified Miso.String  as S
import Optics
import Nostr.Request 
import Nostr.Filter 
import Nostr.Response
import Nostr.Event

import Nostr.WebSocket
import Control.Concurrent
import Control.Monad.IO.Class
import Miso.FFI.WebSocket

import Nostr.Network
import Data.Map as Map
import Control.Monad

start :: JSM ()
start = do 
  nn <- NostrNet <$> (liftIO . newMVar) Map.empty
  let subs = [ websocketConnect nn ["wss://relay.nostrdice.com", "wss://lunchbox.sandwich.farm"] HandleWebSocket ] 
      update = updateModel nn
  startApp App { initialAction = Id, ..}
  where
    model = Model (Message "") mempty "No error yet bitch!"
    events = defaultEvents
    -- subs = [ ]
    view = appView
    -- uri = URL "wss://echo.websocket.org"
    uri = URL "wss://relay.nostrdice.com"
    protocols = Protocols [ ]
    mountPoint = Nothing
    logLevel = Off

updateModel :: NostrNet -> Action -> Model -> Effect Action Model
updateModel nn (HandleWebSocket (WebSocketMessage (resp,rel))) model
  -- = noEff (model & #received .~ S.ms (show evt))
  = noEff $ model & #received %~ (S.ms (show rel <> ": " <> displayResp resp) :)
updateModel _ (HandleWebSocket (WebSocketClose _ _ _)) model = noEff $ model & #err .~ "Connection closed"
updateModel _ (HandleWebSocket (WebSocketError er)) model = noEff $ model & #err .~ er
updateModel nn (HandleWebSocket (WebSocketOpen)) model = 
  let checkAndSend = do 
        ac <- liftIO . allConnected $ nn 
        when ac $ sendAll nn nostrRequest
  in model <# (checkAndSend >> pure Id)
updateModel ms (SendMessage msg) model = model <# do sendAll ms msg >> pure Id
-- updateModel (UpdateMessage m) model = noEff model { msg = Message m }
updateModel _ _ model = noEff model

nostrRequest :: Request
nostrRequest = Subscribe (Subscription [AllNotes] "123")

instance ToJSON Message
instance FromJSON Message

data Model = Model
  { msg :: Message,
    received :: [MisoString],
    err :: MisoString
  }
  deriving (Show, Eq, Generic)

newtype Message = Message MisoString
  deriving (Eq, Show, Generic)

data Action
  = HandleWebSocket (WebSocket (Response, MisoString))
  | SendMessage Message
  -- | UpdateMessage MisoString
  | Id

appView :: Model -> View Action
appView Model{..} = div_ [ style_ $ M.fromList [("text-align", "center")] ] $ 
  [
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/bulma/0.4.3/css/bulma.min.css"]
  , h1_ [style_ $ M.fromList [("font-weight", "bold")] ] [ a_ [ href_ "https://github.com/dmjio/miso" ] [ text $ S.pack "Miso Websocket Example" ] ]
  , h3_ [] [ text $ S.pack "wss://echo.websocket.org" ]
  , input_  [ type_ "text"
            --  , onInput UpdateMessage
            , onEnter (SendMessage msg)
            ]
  , button_ [ onClick (SendMessage msg)
            ] [ text (S.pack "Send to echo server") ]
  ] 
  ++ fmap 
       (\r -> div_ [ ] [ p_ [ ] [ text r | not . S.null $ r ] ]) received
  ++ [div_ [ ] [ p_ [style_ $ M.fromList [("font-weight", "bold")] ] [ text err | not . S.null $ err ] ]]
 
onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool Id action . (== KeyCode 13)

displayResp :: Response -> String
displayResp r = 
  case r of 
    EventReceived _ evt -> (show $ verifySignature evt) <> ": " <> show (evt ^. #content) <> " :: " <> (show $ evt ^. #eventId)<> " :pubkey: " <> (show $ evt ^. #pubKey)
    -- EventReceived _ evt -> (show $ evt ^. #sig) <> " :: " <> (show $ evt ^. #pubKey)
    _ -> show r