{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Miso.Components.LoadingBar where

import Miso.String (ms)
import qualified Data.Map as M
import ModelAction (Page, SubState(..))
import Miso hiding (update, view, at)
import Nostr.Request hiding (Subscribe)
import Optics hiding (view)
import Data.Maybe
import Data.Bool
import GHC.Generics
import qualified Data.Text as T
import Control.Monad.State (get)
import Miso.Concurrent (Waiter(wait))
import Data.Aeson
import Debug.Trace

loadingBarTopic :: Topic Message
loadingBarTopic = topic "loading-bar"

data Message where 
  UpdateSubscriptions :: Page -> (SubscriptionId, SubState) -> Message
  UpdatePage :: Page -> Message
  deriving (Generic, FromJSON, ToJSON)

data Action where
  Subscribe :: Action
  MessageReceived :: (Result Message) -> Action


data Model = Model {
  subscriptions :: M.Map Page [(SubscriptionId, SubState)],
  page :: Maybe Page
} deriving (Generic, Eq)

update :: Action -> Effect Model Action
update a = do
    m <- get
    case a of
      MessageReceived (Success (UpdateSubscriptions p sst)) -> do 
        put $
          m & #subscriptions % at p
              %~ Just . fromMaybe [sst] . fmap (updateSubStates sst)

      MessageReceived (Success (UpdatePage p)) -> do
        put $ m & #page ?~ p

      MessageReceived (Error e) -> io_ $ consoleError $ "LoadingBarAction: Error: " <> ms e

      Subscribe -> subscribe loadingBarTopic MessageReceived

updateSubStates :: Eq a => (a, SubState) -> [(a, SubState)] -> [(a, SubState)]
updateSubStates (sid, ss) substates =
  -- update "sub state" for sid and remove all finished "sub states"
  (sid, ss) : filter (\(sid2, ss2) -> sid2 /= sid && isRunning ss2) substates
  where
     isRunning (SubRunning _) = True
     isRunning _ = False

loadingBarHtml :: View action
loadingBarHtml = rawHtml $
 "<div class=\"lb-container\">\
  \<div class=\"lb-progress lb-progress-infinite\">\
    \<div class=\"lb-progress-bar3\">\
    \</div>\
  \</div>\
\</div>"

view :: Model -> View Action
view m =
  div_
    [ bool
        (class_ "remove-element")
        (class_ "visible")
        $ areSubsRunning m
    ]
    [loadingBarHtml]

areSubsRunning :: Model -> Bool
areSubsRunning m =
  fromMaybe False $ do
    page <- m ^. #page
    subs <- (m ^. #subscriptions) ^. at page
    let isRunning (_, SubRunning _) = True
        isRunning (_, _) = False
    pure . any isRunning $ subs

loadingBar :: Component Model Action
loadingBar = (component (Model M.empty Nothing) update view) {initialAction = Just Subscribe}
