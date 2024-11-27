{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tabs where

import qualified Data.Text as T
import Miso
import qualified LocalStorage
import Control.Monad.IO.Class (MonadIO)

data TabConfig = TabConfig
  { leftContent :: View Action
  , rightContent :: View Action
  , leftLabel :: T.Text
  , rightLabel :: T.Text
  , activeTab :: T.Text
  , customStyle :: Maybe (Attribute Action)
  }

data TabState = TabState
  { currentTab :: T.Text
  , isAnimating :: Bool
  }

initialTabState :: TabState
initialTabState = TabState
  { currentTab = "left-tab"
  , isAnimating = False
  }

displayTabs :: TabConfig -> View Action
displayTabs TabConfig{..} = 
  div_ [class_ "tabs", maybe id id customStyle] $ 
    [ tabView "left-tab" leftLabel leftContent
    , tabView "right-tab" rightLabel rightContent
    ]
  where
    tabView cls label content =
      div_ [class_ cls, onClick (switchTab cls)]
        [ div_ [class_ $ cls <> "-header"] [text label]
        , div_ [class_ $ cls <> if isActive cls then " active" else ""] [content]
        ]

    isActive :: T.Text -> Bool
    isActive tab = activeTab == tab

    switchTab :: MonadIO m => T.Text -> m ()
    switchTab newTab = do
      setState (\s -> s { currentTab = newTab, isAnimating = True })
      LocalStorage.setItem "activeTab" newTab
      void $ setTimeout 300 $ setState (\s -> s { isAnimating = False })
      where void = (>> return ())