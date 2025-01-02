{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Relay.Relay where

import Control.Concurrent (MVar, readMVar)
import Control.Monad (forever, unless)
import Data.Aeson
import qualified Network.WebSockets as WS
import Nostr.Response
import Relay.Database
import Relay.Request

runRelay :: MVar DB -> IO ()
runRelay mdb = do
  print "here"
  WS.runServer "127.0.0.1" 80 $ application mdb

application :: MVar DB -> WS.ServerApp
application mdb pending = do
  conn <- WS.acceptRequest pending
  print $ "Connection established... "
  WS.withPingThread conn 30 (return ()) $ do
    forever $ do
      msg <- WS.receiveData conn
      db <- readMVar mdb
      let newRequest = eitherDecode @Request msg
      print $ "Request received: " <> show newRequest
      case newRequest of
        Left error -> print $ "Error while decoding: " <> show error
        Right (SendEvent e) -> print $ "SendEvent request: not implemented yet"
        Right (Close sid) -> print $ "Close request: not implemented yet"
        Right (Subscribe s) -> do
          let result = runFilters db (filters s)
          let eose = encode $ EOSE (subId s)
          unless (null result) $ do
            mapM_ (WS.sendTextData conn) $ encode . EventReceived (subId s) <$> result
          WS.sendTextData conn eose
