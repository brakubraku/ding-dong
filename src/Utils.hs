{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Utils where 

import Miso
import GHC.Generics
import Control.Concurrent
import Language.Javascript.JSaddle
import Data.Time
import Control.Monad.IO.Class
import Data.Text 

newtype Seconds = Seconds
  { getSeconds :: Float
  }
  deriving (Eq, Generic)
  deriving newtype (Ord)

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

reloadPage :: JSM ()
reloadPage = do
  _ <- getLocation # ("reload" :: String) $ ()
  pure ()

getLocation :: JSM JSVal
getLocation = jsg ("window" :: String) ! ("location" :: String)

lastNotifStorageId :: Text
lastNotifStorageId = "last-notif-date"

loadLastNotif :: JSM UTCTime 
loadLastNotif = do 
  now <- liftIO getCurrentTime
  ln <- getLocalStorage lastNotifStorageId
  case ln of
    Right r -> pure r
    Left _ -> do
      saveLastNotif now
      pure now

saveLastNotif :: UTCTime -> JSM ()
saveLastNotif when = 
  setLocalStorage lastNotifStorageId when

showt :: Show a => a -> Text
showt = pack . show