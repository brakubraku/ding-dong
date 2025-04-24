{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Miso.Components.LoadingBar where

import qualified Data.Map as M
import ModelAction (Page, SubState(..))
import Miso hiding (update, view, at)
import Nostr.Request
import Optics hiding (view)
import Data.Maybe
import Data.Bool
import GHC.Generics
import qualified Data.Text as T
import Control.Monad.State (get)

data Action where 
  UpdateSubscriptions :: Page -> (SubscriptionId, SubState) -> Action
  UpdatePage :: Page -> Action

data Model = Model {
  subscriptions :: M.Map Page [(SubscriptionId, SubState)],
  page :: Maybe Page
} deriving (Generic, Eq)

update :: Action -> Effect Model Action
update a = do
    m <- get 
    case a of 
      UpdateSubscriptions p sst ->
        noEff $
          m & #subscriptions % at p
              %~ Just . fromMaybe [sst] . fmap (updateSubStates sst)
      UpdatePage p ->
        noEff $ m & #page ?~ p 

updateSubStates :: Eq a => (a, SubState) -> [(a, SubState)] -> [(a, SubState)]
updateSubStates (sid, ss) substates =
  -- update "sub state" for sid and remove all finished "sub states"
  (sid, ss) : filter (\(sid2, ss2) -> sid2 /= sid && isRunning ss2) substates
  where 
     isRunning (SubRunning _) = True
     isRunning _ = False

loadingBar :: View action
loadingBar = rawHtml . T.pack $ 
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
    [loadingBar]

areSubsRunning :: Model -> Bool
areSubsRunning m =
  fromMaybe False $ do
    page <- m ^. #page
    subs <- (m ^. #subscriptions) ^. at page
    let isRunning (_, SubRunning _) = True
        isRunning (_, _) = False
    pure . any isRunning $ subs

loadingBarApp :: App Model Action
loadingBarApp = defaultApp (Model M.empty Nothing) update view

loadingBarComponent :: Component Model Action
loadingBarComponent = component "loading-bar" loadingBarApp 
