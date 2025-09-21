{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Miso.Components.AuthorInfoPanel (authorInfoComponent) where

import Miso hiding (style_)
import Miso.String (MisoString, ms, null)
import Miso.Style (style_)
import Prelude hiding (null)
import Data.Monoid ((<>))

authorInfoComponent :: Component Model Action
authorInfoComponent = (component initialModel updateModel viewModel) 

-- Model to store all data
data Model = Model
  { userName :: MisoString
  , userTitle :: MisoString
  , professionalGroups :: [Group]
  , socialGroups :: [Group]
  } deriving (Eq, Show)

data Group = Group
  { groupName :: MisoString
  , groupScore :: Int
  } deriving (Eq, Show)

-- Initial model with sample data
initialModel :: Model
initialModel = Model
  { userName = "Alex Johnson"
  , userTitle = "Software Developer & Community Lead"
  , professionalGroups =
      [ Group "Web Development" 92
      , Group "Data Science" 87
      , Group "UI/UX Design" 78
      , Group "Mobile Development" 85
      ]
  , socialGroups =
      [ Group "Outdoor Adventures" 95
      , Group "Book Club" 88
      , Group "Music Ensemble" 76
      , Group "Foodie Group" 91
      ]
  }

-- Calculate total groups
totalGroups :: Model -> Int
totalGroups model = length (professionalGroups model) + length (socialGroups model)

-- Calculate average score
averageScore :: Model -> Double
averageScore model =
  let allGroups = professionalGroups model ++ socialGroups model
      total = sum (map groupScore allGroups)
      count = length allGroups
  in if count > 0 then fromIntegral total / fromIntegral count else 0.0

-- Main view function
viewModel :: Model -> View action
viewModel model = div_
  [ style_ containerStyle ]
  [ div_
    [ style_ panelStyle ]
    [ userHeaderView model
    , groupsSectionView model
    , footerView model
    ]
  ]

-- User header view
userHeaderView :: Model -> View action
userHeaderView model = div_
  [ style_ headerStyle ]
  [ div_
    [ style_ userNameStyle ]
    [ text (userName model) ]
  , div_
    [ style_ userTitleStyle ]
    [ text (userTitle model) ]
  ]

-- Groups section view
groupsSectionView :: Model -> View action
groupsSectionView model = div_
  [ style_ groupsContainerStyle ]
  [ categoryView "Professional Groups" (professionalGroups model) professionalCategoryStyle
  , categoryView "Social & Community Groups" (socialGroups model) socialCategoryStyle
  ]

-- Category view
categoryView :: MisoString -> [Group] -> [ (MisoString, MisoString) ] -> View action
categoryView title groups categoryStyle = div_
  [ style_ categoryContainerStyle ]
  [ div_
    [ style_ categoryStyle ]
    [ text title ]
  , ul_
    [ style_ listStyle ]
    (map groupItemView groups)
  ]

-- Group item view
groupItemView :: Group -> View action
groupItemView group = li_
  [ style_ itemStyle ]
  [ div_
    [ style_ groupNameStyle ]
    [ text (groupName group) ]
  , div_
    [ style_ groupScoreStyle ]
    [ text (ms (groupScore group)) ]
  ]

-- Footer view
footerView :: Model -> View action
footerView model = div_
  [ style_ footerStyle ]
  [ div_
    []
    [ text "Total groups: "
    , span_
      [ style_ highlightStyle ]
      [ text (ms (totalGroups model)) ]
    , text " | Average score: "
    , span_
      [ style_ highlightStyle ]
      [ text (ms (averageScore model)) ]
    ]
  , div_
    []
    [ text "Last updated: Today" ]
  ]

-- Styles
containerStyle :: [ (MisoString, MisoString) ]
containerStyle =
  [ ("display", "flex")
  , ("justify-content", "center")
  , ("align-items", "center")
  , ("min-height", "100vh")
  -- , ("background", "linear-gradient(135deg, #e0c3fc 0%, #8ec5fc 100%)") -- purple gradient
  , ("padding", "20px")
  , ("padding-top", "0px")
  ]

panelStyle :: [ (MisoString, MisoString) ]
panelStyle =
  [ ("width", "100%")
  , ("max-width", "450px")
  , ("background-color", "rgb(104, 101, 101)") -- light purple
  , ("border-radius", "12px")
  , ("box-shadow", "0 10px 25px rgba(128, 0, 128, 0.1)") -- purple shadow
  , ("overflow", "hidden")
  ]

headerStyle :: [ (MisoString, MisoString) ]
headerStyle =
  [ ("background", "linear-gradient(135deg, #a4508b 0%, #5f0a87 100%)") -- purple gradient
  , ("color", "white")
  , ("padding", "25px")
  , ("text-align", "center")
  ]

userNameStyle :: [ (MisoString, MisoString) ]
userNameStyle =
  [ ("font-size", "24px")
  , ("font-weight", "600")
  , ("margin-bottom", "5px")
  , ("color", "black")
  ]

userTitleStyle :: [ (MisoString, MisoString) ]
userTitleStyle =
  [ ("font-size", "14px")
  , ("opacity", "0.9")
  , ("color", "black")
  ]

groupsContainerStyle :: [ (MisoString, MisoString) ]
groupsContainerStyle =
  [ ("padding", "0 25px")
  ]

categoryContainerStyle :: [ (MisoString, MisoString) ]
categoryContainerStyle =
  [ ("margin", "25px 0")
  ]

professionalCategoryStyle :: [ (MisoString, MisoString) ]
professionalCategoryStyle =
  [ ("font-size", "18px")
  , ("color", "#5f0a87") -- dark purple
  , ("margin-bottom", "15px")
  , ("padding-bottom", "8px")
  , ("border-bottom", "2px solid rgba(95, 10, 135, 0.2)") -- purple
  ]

socialCategoryStyle :: [ (MisoString, MisoString) ]
socialCategoryStyle =
  [ ("font-size", "18px")
  , ("color", "#5f0a87") -- dark purple
  , ("margin-bottom", "15px")
  , ("padding-bottom", "8px")
  , ("border-bottom", "2px solid rgba(95, 10, 135, 0.2)") -- purple
  ]

listStyle :: [ (MisoString, MisoString) ]
listStyle =
  [ ("list-style", "none")
  ]

itemStyle :: [ (MisoString, MisoString) ]
itemStyle =
  [ ("display", "flex")
  , ("justify-content", "space-between")
  , ("align-items", "center")
  , ("padding", "12px 15px")
  , ("margin-bottom", "10px")
  -- , ("background-color", "#f3e6ff") -- light purple
  , ("border-radius", "8px")
  , ("transition", "all 0.2s ease")
  ]

groupNameStyle :: [ (MisoString, MisoString) ]
groupNameStyle =
  [ ("font-size", "16px")
  , ("color", "#5f0a87") -- dark purple
  ]

groupScoreStyle :: [ (MisoString, MisoString) ]
groupScoreStyle =
  [ ("display", "flex")
  , ("align-items", "center")
  , ("justify-content", "center")
  , ("width", "36px")
  , ("height", "36px")
  , ("background", "linear-gradient(135deg, #a4508b 0%, #5f0a87 100%)") -- purple gradient
  , ("color", "#5f0a87") -- dark purple font
  , ("border-radius", "50%")
  , ("font-weight", "600")
  , ("font-size", "14px")
  ]

socialScoreStyle :: [ (MisoString, MisoString) ]
socialScoreStyle =
  [ ("display", "flex")
  , ("align-items", "center")
  , ("justify-content", "center")
  , ("width", "36px")
  , ("height", "36px")
  , ("background", "linear-gradient(135deg, #e0c3fc 0%, #a4508b 100%)") -- purple gradient
  , ("color", "#5f0a87") -- dark purple font
  , ("border-radius", "50%")
  , ("font-weight", "600")
  , ("font-size", "14px")
  ]

footerStyle :: [ (MisoString, MisoString) ]
footerStyle =
  [ ("padding", "18px")
  , ("text-align", "center")
  , ("font-size", "13px")
  , ("color", "#5f0a87") -- dark purple
  , ("border-top", "1px solid #e0c3fc") -- purple
  , ("background", "#f3e6ff") -- light purple
  ]

highlightStyle :: [ (MisoString, MisoString) ]
highlightStyle =
  [ ("color", "#5f0a87") -- dark purple
  , ("font-weight", "500")
  ]

-- Main application
data Action = NoOp

updateModel :: Action -> Effect Model Action
updateModel NoOp = pure ()