{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test where

import Nostr.Event
import Nostr.Keys
import Language.Javascript.JSaddle (JSM, runJSM, askJSM)
import Control.Concurrent (threadDelay, forkIO, newMVar, readMVar, MVar, putMVar, killThread, newEmptyMVar)
import qualified Data.Text as T
import System.Process (createProcess, proc, terminateProcess, CreateProcess(..), StdStream(UseHandle))
import Control.Exception (catch, AsyncException(..))
import System.IO (openFile, stdout, stderr, IOMode(WriteMode), hPutStrLn, hPutStr, Handle)
import GHC.IO.Handle (hDuplicateTo, hDuplicate, hFlush)
import System.Directory (setCurrentDirectory)

import StoredRelay
import Nostr.Network
import DingDong (initAndStart)
import Miso (startComponentForTest, Sink)
import ModelAction
import Relay.RelayServer
import Relay.Database
import Language.Javascript.JSaddle.Warp as Warp
import qualified Data.Sequence as Seq
import Control.Concurrent.Async (race)
import qualified Relay.Request as Relay
import Data.Either.Extra (mapLeft)
import Control.Monad.Reader
import qualified Data.Map as Map
import Puppet
import Configuration

data TestContext = TestContext {
  requestLog :: MVar (Seq.Seq Relay.Request),
  sink :: Sink Action,
  hStdOut :: Handle
}

withDingDong :: 
 Keys ->
 [Event] ->
 Map.Map String String -> 
 ReaderT TestContext JSM a ->
 IO ()
withDingDong keys relayEvents localStorage runTests = do 
  requestLog <- newMVar Seq.empty

  -- start relay
  rt <- forkIO $ do 
      putStrLn "=== Starting Nostr relay ==="
      mdb <- newMVar $ buildTestDB relayEvents
      runRelay mdb requestLog defaultRelayPort

  -- start puppeteer
  pt <- forkIO $ do
      threadDelay 3000000  -- Wait 3 seconds for jsaddle-warp to start
      Test.runHeadlessClient localStorage

  shutdownTrigger <- newEmptyMVar
  -- start warp
  wt <- forkIO $ do 
      putStrLn "=== Starting Warp ==="
      Warp.run defaultWarpPort $ do
        (ostdout, ostderr) <- liftIO $ do
          origOut <- hDuplicate stdout
          origErr <- hDuplicate stderr
          
          logHandle <- openFile "test-warp.log" WriteMode
          hDuplicateTo logHandle stdout
          hDuplicateTo logHandle stderr

          -- devNull <- openFile "/dev/null" WriteMode
          -- hDuplicateTo devNull stdout
          -- hDuplicateTo devNull stderr

          pure (origOut, origErr)
        -- liftIO $ unmute ostdout ostderr
        let relays =
              newActiveRelay . newRelay
                <$> ["ws://127.0.0.1:" <> T.pack (show defaultRelayPort)]
        sink <- initAndStart (keys, True) relays startComponentForTest
        flip runReaderT TestContext {hStdOut=ostdout,..} $ runTests
        let terminate = liftIO . putMVar shutdownTrigger $ ()
        terminate

  -- wait for shutdown
  readMVar shutdownTrigger
  mapM_ killThread [rt, pt, wt]

-- Function to run the headless client with custom localStorage
runHeadlessClient :: Map.Map String String -> IO ()
runHeadlessClient localStorageMap = do
  putStrLn "=== Starting Puppeteer ==="
  setCurrentDirectory "test/puppeteer"
  -- Generate client.js with custom localStorage
  _ <- writePuppeteerClient localStorageMap
  devNull <- openFile "/dev/null" WriteMode
  logHandle <- openFile "test-puppeteer.log" WriteMode
  let cp = (proc "node" ["client.generated.js"]) {
              std_out = UseHandle logHandle,
              std_err = UseHandle logHandle
            }
  (_, _, _, ph) <- createProcess cp
  let waitLoop = do
        threadDelay 1000000
        waitLoop
  waitLoop `catch` \ThreadKilled -> do
    putStrLn "ThreadKilled caught, terminating Puppeteer client."
    terminateProcess ph

-- | Runs a JSM action and returns either its result or an error message if
-- the action does not complete within the given time.j
-- The timeout is specified in microseconds.
--
-- Example usage:
--   timeout 2000000 "Timed out" myAction
--
-- If 'myAction' completes within 2 seconds, returns @Right result@.
-- If it does not, returns @Left "Timed out"@.
--
-- Parameters:
--   * Int: Timeout duration in microseconds.
--   * String: Error message to return if timeout occurs.
--   * JSM a: The action to run.
--
-- Returns: JSM (Either String a) — Either the error message or the result of the action.
timeout :: Int -> String -> JSM a -> JSM (Either String a)
timeout microseconds errorMsg action = do 
  ctx <- askJSM 
  liftIO $ 
    mapLeft (const errorMsg) <$> 
      race (threadDelay microseconds) 
           (runJSM action ctx)

pollUntilTrue :: MVar a -> Int -> (a -> Bool) -> JSM ()
pollUntilTrue mVar interval check = liftIO $
  let poll = do 
        var <- readMVar mVar
        if check var 
          then pure () 
          else threadDelay interval >> poll
  in poll

timeoutTest :: String -> Int -> String -> JSM a -> ReaderT TestContext JSM () 
timeoutTest testName i e a = do
  ctx <- ask 
  lift $ runTest ctx $ timeout i e a
 where 
  runTest :: TestContext -> JSM (Either String a) -> JSM ()
  runTest TestContext{..} test = do 
    let printLn = hPutStrLn hStdOut
        print = hPutStr hStdOut
    liftIO $ print $ testName <> " ........ "
    result <- test 
    liftIO $ case result of 
      Left errorMsg -> print errorMsg 
      Right _ -> printLn "OK"
    liftIO $ hFlush hStdOut
  
-- mute :: IO  
mute = do
  devNull <- openFile "/dev/null" WriteMode
  origOut <- hDuplicate stdout
  origErr <- hDuplicate stderr
  hDuplicateTo devNull stdout
  hDuplicateTo devNull stderr
  pure (origOut, origErr)

unmute origOut origErr =
  do 
    hDuplicateTo origOut stdout
    hDuplicateTo origErr stderr
