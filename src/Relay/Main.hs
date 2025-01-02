module Main where 

import Relay.RelayServer
import Relay.Database
import Control.Concurrent

main :: IO ()
main = do 
  mdb <- newMVar emptyDB
  runRelay mdb