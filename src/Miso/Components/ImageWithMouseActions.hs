-- image component which changes it's attributes when you hoverOn/hoverOff over it
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Miso.Components.ImageWithMouseActions (imgWithMouseActions) where

import Miso hiding (view, update)
import GHC.Generics
import Control.Monad.State (put)
import Debug.Trace (trace)

data Action = HoverOn | HoverOff

instance Show Action where
  show HoverOn   = "ImageWithMouseActions: HoverOn"
  show HoverOff  = "ImageWithMouseActions: HoverOff"

data Model = HoveredOn | NotHoveredOn deriving (Generic, Eq)
 
update :: Action -> Effect Model Action
update HoverOn   = put HoveredOn 
update HoverOff  = put NotHoveredOn 

view :: [Attribute Action] -> [Attribute Action] -> Model -> View Action
view hoveredOnProps defaultProps m = 
      div_ [onMouseEnter HoverOn, onMouseOut HoverOff] $
        case m of 
            HoveredOn -> [img_ $ hoveredOnProps]
            NotHoveredOn -> [img_ $ defaultProps]
    -- ]
imgWithMouseActions :: [Attribute Action] -> [Attribute Action] -> App name Model Action
imgWithMouseActions hoveredOnProps defaultProps =
  (defaultApp
    NotHoveredOn
    update
    (view hoveredOnProps defaultProps)) { events = defaultEvents <> mouseEvents, logLevel = DebugAll}
