{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module Miso.Components.Post (postComponent) where

import Miso hiding (style_)
import Miso.String (MisoString, ms, null)
import Miso.Style (style_)
import Prelude hiding (null)

postComponent :: Component Model Action
postComponent = (component initialModel updateModel viewModel) 

-- Model for our application state
data Model = Model
  { isLiked :: Bool
  , likeCount :: Int
  , commentCount :: Int
  , isCommentSectionVisible :: Bool
  , commentText :: MisoString
  , comments :: [MisoString]
  } deriving (Show, Eq)

-- Initial state
initialModel :: Model
initialModel = Model
  { isLiked = False
  , likeCount = 127
  , commentCount = 23
  , isCommentSectionVisible = False
  , commentText = ""
  , comments = ["Looks amazing! Which trail is this?", "I was there last weekend! The view is even better in person."]
  }

-- Actions to update our model
data Action
  = ToggleLike
  | ToggleCommentSection
  | UpdateCommentText MisoString
  | AddComment
  | NoOp

-- Independent style functions
containerStyle :: [(MisoString, MisoString)]
containerStyle = 
  [ ("backgroundColor", "#fff")
  , ("borderRadius", "12px")
  , ("boxShadow", "0 2px 16px rgba(0, 0, 0, 0.1)")
  , ("width", "100%")
  , ("maxWidth", "500px")
  , ("overflow", "hidden")
  , ("margin", "20px auto")
  ]

headerStyle :: [(MisoString, MisoString)]
headerStyle = 
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("padding", "16px")
  , ("borderBottom", "1px solid #e4e6eb")
  ]

profilePicStyle :: [(MisoString, MisoString)]
profilePicStyle = 
  [ ("width", "40px")
  , ("height", "40px")
  , ("borderRadius", "50%")
  , ("backgroundColor", "#ddd")
  , ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("marginRight", "12px")
  , ("fontSize", "20px")
  ]

authorInfoStyle :: [(MisoString, MisoString)]
authorInfoStyle = 
  [ ("flex", "1")
  ]

authorNameStyle :: [(MisoString, MisoString)]
authorNameStyle = 
  [ ("fontWeight", "600")
  , ("color", "#050505")
  , ("marginBottom", "2px")
  ]

postTimeStyle :: [(MisoString, MisoString)]
postTimeStyle = 
  [ ("fontSize", "13px")
  , ("color", "#65676b")
  ]

contentStyle :: [(MisoString, MisoString)]
contentStyle = 
  [ ("padding", "16px")
  , ("color", "#050505")
  , ("lineHeight", "1.4")
  ]

imagePlaceholderStyle :: [(MisoString, MisoString)]
imagePlaceholderStyle = 
  [ ("background", "linear-gradient(135deg, #43cea2, #185a9d)")
  , ("height", "200px")
  , ("borderRadius", "8px")
  , ("marginTop", "15px")
  , ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("color", "white")
  , ("fontSize", "20px")
  ]

statsStyle :: [(MisoString, MisoString)]
statsStyle = 
  [ ("display", "flex")
  , ("justifyContent", "space-between")
  , ("padding", "12px 16px")
  , ("color", "#65676b")
  , ("fontSize", "14px")
  , ("borderBottom", "1px solid #e4e6eb")
  ]

actionsStyle :: [(MisoString, MisoString)]
actionsStyle = 
  [ ("display", "flex")
  , ("justifyContent", "space-around")
  , ("padding", "8px 0")
  , ("borderBottom", "1px solid #e4e6eb")
  ]

buttonStyle :: [(MisoString, MisoString)]
buttonStyle = 
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("background", "none")
  , ("border", "none")
  , ("borderRadius", "4px")
  , ("padding", "8px 16px")
  , ("cursor", "pointer")
  , ("color", "#65676b")
  , ("fontWeight", "600")
  , ("transition", "all 0.2s")
  , ("flex", "1")
  , ("margin", "0 4px")
  ]

activeLikeStyle :: [(MisoString, MisoString)]
activeLikeStyle = 
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("background", "none")
  , ("border", "none")
  , ("borderRadius", "4px")
  , ("padding", "8px 16px")
  , ("cursor", "pointer")
  , ("color", "#1877f2")
  , ("fontWeight", "600")
  , ("transition", "all 0.2s")
  , ("flex", "1")
  , ("margin", "0 4px")
  ]

activeCommentStyle :: [(MisoString, MisoString)]
activeCommentStyle = 
  [ ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("background", "none")
  , ("border", "none")
  , ("borderRadius", "4px")
  , ("padding", "8px 16px")
  , ("cursor", "pointer")
  , ("color", "#42b883")
  , ("fontWeight", "600")
  , ("transition", "all 0.2s")
  , ("flex", "1")
  , ("margin", "0 4px")
  ]

commentStyle :: [(MisoString, MisoString)]
commentStyle = 
  [ ("display", "flex")
  , ("marginTop", "16px")
  ]

commentAvatarStyle :: [(MisoString, MisoString)]
commentAvatarStyle = 
  [ ("width", "32px")
  , ("height", "32px")
  , ("borderRadius", "50%")
  , ("backgroundColor", "#e4e6eb")
  , ("marginRight", "10px")
  , ("flexShrink", "0")
  , ("display", "flex")
  , ("alignItems", "center")
  , ("justifyContent", "center")
  , ("fontSize", "14px")
  ]

commentContentStyle :: [(MisoString, MisoString)]
commentContentStyle = 
  [ ("background", "#f0f2f5")
  , ("padding", "10px 12px")
  , ("borderRadius", "18px")
  , ("flex", "1")
  ]

commentAuthorStyle :: [(MisoString, MisoString)]
commentAuthorStyle = 
  [ ("fontWeight", "600")
  , ("fontSize", "14px")
  , ("marginBottom", "4px")
  ]

commentTextStyle :: [(MisoString, MisoString)]
commentTextStyle = 
  [ ("fontSize", "14px")
  , ("color", "#050505")
  ]

commentInputStyle :: [(MisoString, MisoString)]
commentInputStyle = 
  [ ("display", "flex")
  , ("marginTop", "12px")
  , ("alignItems", "center")
  ]

inputStyle :: [(MisoString, MisoString)]
inputStyle = 
  [ ("flex", "1")
  , ("padding", "10px 12px")
  , ("border", "1px solid #dddfe2")
  , ("borderRadius", "20px")
  , ("outline", "none")
  , ("fontSize", "14px")
  ]

submitButtonStyle :: [(MisoString, MisoString)]
submitButtonStyle = 
  [ ("background", "#1877f2")
  , ("color", "white")
  , ("border", "none")
  , ("borderRadius", "20px")
  , ("padding", "10px 16px")
  , ("marginLeft", "8px")
  , ("cursor", "pointer")
  , ("fontWeight", "600")
  ]

-- Update function to handle actions
updateModel :: Action -> Effect Model Action
updateModel action = do 
  model <- get 
  case action of
    ToggleLike -> noEff $ model
      { isLiked = not (isLiked model)
      , likeCount = if not (isLiked model) then likeCount model + 1 else likeCount model - 1
      }
    
    ToggleCommentSection -> noEff $ model
      { isCommentSectionVisible = not (isCommentSectionVisible model)
      }
    
    UpdateCommentText text -> noEff $ model
      { commentText = text
      }
    
    AddComment -> noEff $ model
      { comments = if  null (commentText model) 
                  then comments model 
                  else comments model ++ [commentText model]
      , commentCount = if null (commentText model) 
                      then commentCount model 
                      else commentCount model + 1
      , commentText = ""
      }
    
    NoOp -> noEff model

-- View function to render the UI
viewModel :: Model -> View Action
viewModel model = div_
  [ 
    -- style_
    --   [ ("backgroundColor", "#f0f2f5")
    --   , ("display", "flex")
    --   , ("justifyContent", "center")
    --   , ("alignItems", "center")
    --   , ("minHeight", "100vh")
    --   , ("padding", "20px")
    --   , ("fontFamily", "'Segoe UI', Roboto, Arial, sans-serif")
    --   ]
  ]
  [ div_ [style_ containerStyle] -- Post container
      [ div_ [style_ headerStyle] -- Header
          [ div_ [style_ profilePicStyle] [text "👤"] -- Profile picture
          , div_ [style_ authorInfoStyle] -- Author info
              [ div_ [style_ authorNameStyle] [text "Alex Morgan"]
              , div_ [style_ postTimeStyle] [text "Posted 5 hours ago"]
              ]
          ]
      
      , div_ [style_ contentStyle] -- Content
          [ p_ [] [text "Just finished my morning hike with an amazing view\
          \from the top! 🏞️ There's nothing like fresh mountain air to start the day. Who else loves hiking on weekends?"]
          , div_ [style_ imagePlaceholderStyle] [text "🏔️ Mountain View"]
          ]
      
      , div_ [style_ statsStyle] -- Stats
          [ div_ [] [text $ "👍 " <> ms (show (likeCount model))]
          , div_ [] [text $ ms (show (commentCount model)) <> " comments"]
          ]
      
      , div_ [style_ actionsStyle] -- Actions
          [ button_
              [ style_ (if isLiked model then activeLikeStyle else buttonStyle)
              , onClick ToggleLike
              ]
              [text $ if isLiked model then "Liked" else "Like"]
          , button_
              [ style_ (if isCommentSectionVisible model then activeCommentStyle else buttonStyle)
              , onClick ToggleCommentSection
              ]
              [text "Comment"]
          ]
      
      , div_ -- Comments section (conditionally visible)
          [ style_
              [ ("padding", "16px")
              , ("display", if isCommentSectionVisible model then "block" else "none")
              ]
          ]
          [ -- Existing comments
            div_ [] (map (\comment -> 
              div_ [style_ commentStyle]
                [ div_ [style_ commentAvatarStyle] [text "👤"]
                , div_ [style_ commentContentStyle]
                    [ div_ [style_ commentAuthorStyle] [text "User"]
                    , div_ [style_ commentTextStyle] [text comment]
                    ]
                ]
            ) (comments model))
          
          , div_ [style_ commentInputStyle] -- Comment input
              [ input_
                  [ style_ inputStyle
                  , type_ "text"
                  , placeholder_ "Write a comment..."
                  , value_ (commentText model)
                  , onInput UpdateCommentText
                  , onEnter AddComment
                  ]
              , button_
                  [ style_ submitButtonStyle
                  , onClick AddComment
                  ]
                  [text "Post"]
              ]
          ]
      ]
  ]

-- Helper for handling Enter key in input
onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ \key -> if key == KeyCode 13 then action else NoOp
