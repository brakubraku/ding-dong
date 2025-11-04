{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Relay.RelayServer where

import Control.Concurrent (MVar, readMVar, modifyMVar_)
import Data.Sequence (Seq, (|>))
import qualified Data.Sequence as Seq
import Control.Monad (forever, unless, when)
import Data.Aeson
import qualified Network.WebSockets as WS
import Nostr.Response
import Relay.Database
import Relay.Request

import Debug.Trace

import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler (setFormatter)
import System.Log.Formatter
import System.Directory (doesFileExist, removeFile)

runRelay :: MVar DB -> MVar (Seq Request) -> Int -> IO ()
runRelay mdb requestLog port = do
  logger <- createLogger
  WS.runServer "127.0.0.1" port $ application logger mdb requestLog

createLogger = do 
  let logFile = "relay-server.log"
  exists <- doesFileExist logFile
  when exists $ removeFile logFile
  logger <- setLevel INFO <$> getLogger "RelayServer"
  fh <- fileHandler logFile DEBUG >>= \lh -> 
             pure $ setFormatter lh (simpleLogFormatter "[$time] $msg")
  let logger' = addHandler fh logger
  saveGlobalLogger logger'
  pure logger'

application :: Logger -> MVar DB -> MVar (Seq Request) -> WS.ServerApp
application logger mdb requestLog pending = do
  let info = logL logger INFO
      debug = logL logger DEBUG
  debug $ "Nostr relay started. Waiting for connections "
  conn <- WS.acceptRequest pending
  debug $ "Connection established... "
  WS.withPingThread conn 30 (return ()) $ do
    forever $ do
      msg <- WS.receiveData conn
      db <- readMVar mdb
      debug $ "Received json=" <> show msg
      let request = eitherDecode @Request msg
      either
        (\err -> info $ "Error while decoding: " <> show err)
        (\req -> do
            info $ "Request: " <> show req
            modifyMVar_ requestLog $ \reqs -> return (reqs |> req)
            case req of
              SendEvent e -> info $ "SendEvent request: not implemented yet"
              Close sid -> info $ "Close request: not implemented yet"
              Subscribe s -> do
                let result = runFilters db (filters s)
                info $ "Response: " <> show result
                let eose = encode $ EOSE (subId s)
                mapM_ (WS.sendTextData conn) $ encode . EventReceived (subId s) <$> result
                WS.sendTextData conn eose
        )
        request
