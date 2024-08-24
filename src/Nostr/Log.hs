{-# LANGUAGE OverloadedStrings #-}

module Nostr.Log where

import Data.DateTime
import Data.Text

import Nostr.Relay

logError :: Text -> IO ()
logError e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[ERROR] " <> e

logRelayError :: Relay -> Text -> IO ()
logRelayError r e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[ERROR] " <> uri r <> ": " <> e

logInfo :: Text -> IO ()
logInfo e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[INFO] " <> e

logRelayInfo :: Relay -> Text -> IO ()
logRelayInfo r e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[INFO] " <> uri r <> ": " <> e

logDebug :: Text -> IO ()
logDebug e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[DEBUG] " <> e

logRelayDebug :: Relay -> Text -> IO ()
logRelayDebug r e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[DEBUG] " <> uri r <> ": " <> e

logObject :: (Show a) => a -> Text -> IO ()
logObject o e = do
  now <- getCurrentTime
  print $ (pack . show) now <> "[INFO] " <> (pack . show) o <> " " <> e

-- logError :: Text -> IO ()
-- logError e = print $ "[ERROR] " <> e
