{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications #-}

module Utils where 

import Miso
import GHC.Generics
import Control.Concurrent
import Language.Javascript.JSaddle
import Data.Time
import Control.Monad.IO.Class
import Data.Text 
import Control.Monad (when)
import Data.Maybe (isJust)

newtype Seconds = Seconds
  { getSeconds :: Float
  }
  deriving (Eq, Generic, Show)
  deriving newtype (Ord)

sleep :: Seconds -> IO () 
sleep (Seconds s) = threadDelay . round $  s * 10^6

reloadPage :: JSM ()
reloadPage = do
  _ <- getLocation # ("reload" :: String) $ ()
  pure ()

getLocation :: JSM JSVal
getLocation = jsg ("window" :: String) ! ("location" :: String)

lastNotifStorageId :: Text
lastNotifStorageId = pack "last-notif-date"

loadLastNotifTime :: JSM UTCTime 
loadLastNotifTime = do 
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

getValueOfInput :: Text -> JSM Text
getValueOfInput inputId = 
    valToText =<< getElementById inputId ! ("value" :: String)

setValueOfInput :: Text -> Text -> JSM ()
setValueOfInput inputId t = do
    i <- getElementById inputId 
    v <- toJSVal t
    setProp (toJSString "value") v $ Object i

scrollIntoView :: Text -> JSM ()
scrollIntoView elId = do
  el <- jsg "document" # "getElementById" $ [elId]
  isNotNull <- isJust <$> maybeNullOrUndefined el
  when isNotNull $ do 
    el # "scrollIntoView" $ ()
    pure ()

collectJustM :: (MonadIO m) => m (Maybe a) -> m [a]
collectJustM action = do
  x <- action
  case x of
    Nothing -> pure []
    Just x -> do
      xs <- collectJustM action
      return (x : xs)