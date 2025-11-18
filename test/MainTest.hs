{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}

module MainTest where 

import Test
import TestEvents
import Optics
import Control.Monad.Reader
import qualified Data.Map as Map
import qualified Relay.Request as Relay
import Nostr.Kind

import Nostr.Keys
import System.Environment (getArgs)

import qualified UnitTest.PeriodicLoader as PeriodicLoader

runUnit :: IO ()
runUnit = 
  sequence_ [PeriodicLoader.runTests]

runIntegration :: IO ()
runIntegration = do
  newKeys <- generateKeys
  otherKeys <- generateKeys
  withDingDong newKeys [simpleContacts newKeys] Map.empty $ do
      TestContext{..} <- ask

      timeoutTest 
        "See if metadata and relaylist is requested" 
        1000000 
        "Did not find metadata and relaymetadata requests" $ 
          pollUntilTrue requestLog 100000 $ 
             any (findRequest (newKeys ^. #xo))
      
      -- liftIO . threadDelay $ 50 * 1000000
      
findRequest xo (Relay.Subscribe s) = 
  let filters = s ^. #filters
  in  any (relayListFilter xo) filters 
         && any (metadataFilter xo) filters
findRequest _ _ = False

relayListFilter xo f = 
  f ^. #kinds == Just [RelayList] && 
  f ^. #authors == Just [xo]

metadataFilter xo f = 
  f ^. #kinds == Just [Metadata] &&
  f ^. #authors == Just [xo]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["integration"] -> runIntegration
    ["unit"]        -> runUnit
    _                -> do
      putStrLn "Unknown argument. Usage: <integration|unit>"
      putStrLn "Defaulting to integration test."
      runIntegration