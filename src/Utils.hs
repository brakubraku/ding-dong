{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where 

import Miso
import GHC.Generics
import Control.Concurrent

newtype Seconds = Seconds
  { getSeconds :: Float
  }
  deriving (Eq, Generic)
  deriving newtype (Num, Ord)

sleep :: Seconds -> IO () 
sleep (Seconds s) = threadDelay . round $  s * 10^6

loadingBar :: View action
loadingBar = rawHtml $ 
 "<div class=\"lb-container\">\
  \<div class=\"lb-progress lb-progress-infinite\">\
    \<div class=\"lb-progress-bar3\">\
    \</div>\
  \</div>\
\</div>"

