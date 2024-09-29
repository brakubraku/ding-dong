{-# LANGUAGE OverloadedStrings #-}

module Utils where 

import Miso

loadingBar :: View action
loadingBar = rawHtml $ 
 "<div class=\"lb-container\">\
  \<div class=\"lb-progress lb-progress-infinite\">\
    \<div class=\"lb-progress-bar3\">\
    \</div>\
  \</div>\
\</div>"

