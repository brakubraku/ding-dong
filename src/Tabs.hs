{-# LANGUAGE OverloadedStrings #-}

module Tabs where

import qualified Data.Text as T
import Miso

displayTabs :: (View action, View action) -> T.Text -> T.Text -> View action
displayTabs (l, r) leftLabel rightLabel =
  div_ [class_ "tabs"] $
    [ div_
        [class_ "left-tab"]
        [ div_ [class_ "left-tab-header"] [text leftLabel],
          l
        ],
      div_
        [class_ "right-tab"]
        [ div_ [class_ "right-tab-header"] [text rightLabel],
          r
        ]
    ]

