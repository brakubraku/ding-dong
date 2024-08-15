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

import           Miso
import           Miso.String  (MisoString)
import qualified Miso.String  as S
import Optics
import Nostr.Request 
import Nostr.Filter 
import Nostr.Response
import Nostr.Event

start :: JSM ()
start = startApp App { initialAction = Id, ..}
  where
    model = Model (Message "") mempty "No error yet bitch!"
    events = defaultEvents
    subs = [ websocketSub uri protocols HandleWebSocket ]
    -- subs = [ ]
    update = updateModel
    view = appView
    -- uri = URL "wss://echo.websocket.org"
    uri = URL "wss://relay.nostrdice.com"
    protocols = Protocols [ ]
    mountPoint = Nothing
    logLevel = Off

updateModel :: Action -> Model -> Effect Action Model
updateModel (HandleWebSocket (WebSocketMessage evt)) model
  -- = noEff (model & #received .~ S.ms (show evt))
  = noEff $ model & #received %~ (S.ms (displayResp evt) :)
updateModel (HandleWebSocket (WebSocketClose _ _ _)) model = noEff $ model & #err .~ "Connection closed"
updateModel (HandleWebSocket (WebSocketError er)) model = noEff $ model & #err .~ er
updateModel (HandleWebSocket (WebSocketOpen)) model = model <# do send nostrRequest >> pure Id
updateModel (SendMessage msg) model = model <# do send msg >> pure Id
-- updateModel (UpdateMessage m) model = noEff model { msg = Message m }
updateModel _ model = noEff model

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
  = HandleWebSocket (WebSocket Response)
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
    EventReceived _ evt -> show $ evt ^. #content
    _ -> show r