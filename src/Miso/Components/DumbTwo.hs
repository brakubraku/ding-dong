{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Miso.Components.DumbTwo (dumbComponent) where

import Miso hiding (style_)
import Miso.String (MisoString, ms)
import Miso.Style (style_)

dumbComponent :: Component Model Action
dumbComponent = (component initialModel updateModel viewModel) 

-- Model
data Model = Model
  { posts :: [Post]
  } deriving (Show, Eq)

data Post = Post
  { postId :: Int
  , avatar :: MisoString
  , username :: MisoString
  , timestamp :: MisoString
  , content :: MisoString
  , replies :: [Post]
  } deriving (Show, Eq)

-- Initial model
initialModel :: Model
initialModel = Model
  [ Post 1 "JS" "John Smith" "2 hours ago" "Just finished reading an amazing book about web development. Highly recommend it to all developers out there!" []
  , Post 2 "AD" "Amanda Davis" "1 hour ago" "What's everyone's favorite JavaScript framework these days? I'm starting a new project and could use some recommendations." 
      [ Post 3 "TS" "Tom Sanchez" "45 minutes ago" "I've been using Vue.js for the past year and it's been fantastic for both small and large projects!" [] ]
  , Post 4 "RP" "Robert Parker" "30 minutes ago" "Just launched my new portfolio website. Check it out and let me know what you think!" []
  , Post 5 "EJ" "Emma Johnson" "15 minutes ago" "Does anyone have recommendations for good resources to learn advanced CSS techniques?" 
      [ Post 6 "MJ" "Michael James" "5 minutes ago" "CSS-Tricks and Kevin Powell's YouTube channel are both excellent resources for learning advanced CSS!" [] ]
  ]

-- Action
data Action
  = NoOp
  deriving (Show)

-- Update
updateModel :: Action ->  Effect Model Action
updateModel NoOp = pure ()

-- View
viewPost :: Post -> View Action
viewPost post = div_ [ style_ postStyle ]
  [ div_ [ style_ postHeaderStyle ]
      [ div_ [ style_ avatarStyle ] [ text (avatar post) ]
      , div_ [ style_ userInfoStyle ]
          [ div_ [ style_ usernameStyle ] [ text (username post) ]
          , div_ [ style_ timestampStyle ] [ text (timestamp post) ]
          ]
      ]
  , div_ [ style_ postContentStyle ]
      [ p_ [ style_ postTextStyle ] [ text (content post) ]
      , div_ [] (map viewReply (replies post))
      ]
  ]

viewReply :: Post -> View Action
viewReply post = div_ [ style_ replyStyle ]
  [ div_ [ style_ replyIndicatorStyle ] [ text ("Replying to " <> username post) ]
  , div_ [ style_ postHeaderStyle ]
      [ div_ [ style_ avatarStyle ] [ text (avatar post) ]
      , div_ [ style_ userInfoStyle ]
          [ div_ [ style_ usernameStyle ] [ text (username post) ]
          , div_ [ style_ timestampStyle ] [ text (timestamp post) ]
          ]
      ]
  , div_ [ style_ postContentStyle ]
      [ p_ [ style_ postTextStyle ] [ text (content post) ]
      ]
  ]

viewModel :: Model -> View Action
viewModel model = div_ [ style_ feedContainerStyle ]
  (map viewPost (posts model))

-- Inline Styles
feedContainerStyle = 
  [ ("maxWidth", "600px")
  , ("margin", "0 auto")
  , ("backgroundColor", "white")
  , ("borderRadius", "8px")
  , ("boxShadow", "0 2px 10px rgba(0, 0, 0, 0.1)")
  , ("overflow", "hidden")
  ]

postStyle = 
  [ ("padding", "16px")
  , ("borderBottom", "1px solid #eaeaea")
  ]

postHeaderStyle = 
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("marginBottom", "8px")
  ]

avatarStyle = 
  [ ("width", "40px")
  , ("height", "40px")
  , ("borderRadius", "50%")
  , ("backgroundColor", "#ddd")
  , ("marginRight", "12px")
  , ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("fontWeight", "bold")
  , ("color", "#777")
  ]

-- userInfoStyle :: Attribute action
userInfoStyle :: [(MisoString, MisoString)]
userInfoStyle = 
  [ ("flex", "1")
  ]

usernameStyle = 
  [ ("fontWeight", "bold")
  , ("marginBottom", "2px")
  ]

timestampStyle = 
  [ ("fontSize", "0.8rem")
  , ("color", "#777")
  ]

postContentStyle = 
  [ ("marginLeft", "52px")
  ]

postTextStyle = 
  [ ("marginBottom", "8px")
  ]

replyStyle = 
  [ ("marginTop", "16px")
  , ("marginLeft", "32px")
  , ("padding", "12px")
  , ("backgroundColor", "#f9f9f9")
  , ("borderLeft", "3px solid #ddd")
  , ("borderRadius", "0 4px 4px 0")
  ]

replyIndicatorStyle = 
  [ ("fontSize", "0.8rem")
  , ("color", "#777")
  , ("marginBottom", "4px")
  , ("display", "flex")
  , ("alignItems", "center")
  ]
