{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Tabs where

import qualified Data.Text as T
import Miso
import qualified LocalStorage

data TabConfig = TabConfig
  { leftContent :: View action
  , rightContent :: View action
  , leftLabel :: T.Text
  , rightLabel :: T.Text
  , activeTab :: T.Text
  , customStyle :: Maybe Attribute action
  }

data TabState = TabState
  { currentTab :: T.Text
  , isAnimating :: Bool
  }

displayTabs :: TabConfig -> View action
displayTabs TabConfig{..} = 
  div_ [class_ "tabs", customStyle `maybe` id] $ 
    [ tabView "left-tab" leftLabel leftContent
    , tabView "right-tab" rightLabel rightContent
    ]
  where
    tabView cls label content =
      div_ [class_ cls, onClick (switchTab cls)]
        [ div_ [class_ $ cls <> "-header"] [text label]
        , div_ [class_ $ cls <> if isActive cls then " active" else ""] [content]
        ]

    isActive tab = activeTab == tab

    switchTab :: T.Text -> action  
    switchTab newTab = do
      setState (\s -> s { currentTab = newTab, isAnimating = True })
      LocalStorage.setItem "activeTab" newTab
      setTimeout 300 $ setState (\s -> s { isAnimating = False })