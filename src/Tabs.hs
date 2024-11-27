{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module Tabs 
  ( TabConfig(..)
  , TabState(..)
  , initialTabState
  , displayTabs
  , TabAction(..)
  ) where

import qualified Data.Text as T
import Miso
import qualified LocalStorage
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Exception (catch)

data TabConfig = TabConfig
  { leftContent :: View TabAction
  , rightContent :: View TabAction
  , leftLabel :: T.Text
  , rightLabel :: T.Text
  , activeTab :: T.Text
  , customStyle :: Maybe (Attribute TabAction)
  }

data TabState = TabState
  { currentTab :: T.Text
  , isAnimating :: Bool
  }

data TabAction 
  = SwitchTab T.Text
  | SetAnimating Bool
  deriving (Show, Eq)

initialTabState :: TabState
initialTabState = TabState
  { currentTab = "left-tab"
  , isAnimating = False
  }

displayTabs :: TabConfig -> View TabAction
displayTabs TabConfig{..} = 
  div_ [class_ "tabs", maybe id id customStyle] $ 
    [ tabView "left-tab" leftLabel leftContent
    , tabView "right-tab" rightLabel rightContent
    ]
  where
    tabView cls label content =
      div_ [class_ cls, onClick (SwitchTab cls)]
        [ div_ [class_ $ cls <> "-header"] [text label]
        , div_ [class_ $ cls <> if isActive cls then " active" else ""] [content]
        ]

    isActive :: T.Text -> Bool
    isActive tab = activeTab == tab

updateTabState :: MonadIO m => TabAction -> TabState -> m TabState
updateTabState (SwitchTab newTab) state = do
  liftIO $ catch 
    (LocalStorage.setItem "activeTab" newTab)
    (\e -> putStrLn $ "LocalStorage error: " ++ show (e :: IOError))
  return $ state { currentTab = newTab, isAnimating = True }

updateTabState (SetAnimating val) state =
  return $ state { isAnimating = val }