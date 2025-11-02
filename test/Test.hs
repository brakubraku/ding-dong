{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}

module Test where

import TestEvents
import Nostr.Event
import Nostr.Keys
import Control.Monad.IO.Class (liftIO)
import Language.Javascript.JSaddle (JSM, runJSM, askJSM)
import Control.Concurrent (threadDelay, forkIO, newMVar, readMVar, MVar, ThreadId, putMVar)
import Data.Text (Text)
import qualified Data.Text as T
import System.Process (createProcess, proc, terminateProcess, ProcessHandle, CreateProcess(..), StdStream(UseHandle))
import Control.Exception (try, SomeException, catch, throwTo, AsyncException(..))
import Network.HTTP.Conduit
import Network.HTTP.Types.Status (status200)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as C8
import System.IO (openFile, stdout, stderr, IOMode(WriteMode), hPutStrLn, hPutStr, Handle)
import GHC.IO.Handle (hDuplicateTo, hDuplicate, hFlush)
import System.Directory (setCurrentDirectory)
import System.Posix.Signals (installHandler, sigKILL, Handler(Catch))

import StoredRelay
import Nostr.Network
import DingDong (initAndStart)
import Miso (startComponentForTest)
import ModelAction
import Relay.RelayServer
import Relay.Database
import Language.Javascript.JSaddle.Warp as Warp
import qualified Data.Sequence as Seq
import Control.Concurrent.Async (race)
import Data.Either
import Relay.Request
import Nostr.Kind
import Optics
import Nostr.Kind (Kind(Metadata))
import Debug.Trace
import Nostr.Profile (Profile(about))
import Data.Either.Extra (mapLeft)
import Control.Monad.Reader
import qualified Data.Map as Map
import Puppet

data TestCtx = TestCtx {
  hStdOut :: Handle
}

-- Function to run the headless client with custom localStorage
runHeadlessClient :: Bool -> Map.Map String String -> IO ()
runHeadlessClient suppressOutput localStorageMap = do
  putStrLn "=== Starting Puppeteer ==="
  setCurrentDirectory "test/puppeteer"
  -- Generate client.js with custom localStorage
  _ <- writePuppeteerClient localStorageMap
  devNull <- openFile "/dev/null" WriteMode
  let cp = (proc "node" ["client.generated.js"]) {
              std_out = UseHandle devNull,
              std_err = UseHandle devNull
            }
  (_, _, _, ph) <- createProcess cp
  let waitLoop = do
        threadDelay 1000000
        waitLoop
  waitLoop `catch` \ThreadKilled -> do
    putStrLn "ThreadKilled caught, terminating Puppeteer client."
    terminateProcess ph

-- Test group 1
-- test that when triggering some Action, certain requests are sent to the relay/relays

-- how to implement it 
-- startComponent but get the sink from it. 
-- sink the actions you want
-- observe the requests within some timeout

runTest :: MVar () -> IO ()
runTest shutdownTrigger = do
  requestLog <- newMVar Seq.empty
  newKeys <- generateKeys
  let contacts = simpleContacts newKeys
  forkIO $ do 
     putStrLn "=== Starting Nostr relay ==="
     mdb <- newMVar $ buildTestDB [contacts]
     runRelay mdb requestLog 8080
 
  threadDelay 1000000
  let active =
        newActiveRelay . newRelay
          <$> ["ws://127.0.0.1:8080"]

  putStrLn "=== Starting Warp ==="
  Warp.run 1234 $ do
    (ostdout, ostderr) <- liftIO mute -- don't need to see all the debug crap
    -- liftIO $ unmute ostdout ostderr
    sink <- initAndStart (newKeys, True) active startComponentForTest

    flip runReaderT (TestCtx ostdout) $ 
      timeoutTest 
        "See if metadata and relaylist is requested" 
        1000000 
        "Did not find metadata and relaymetadata requests" $ 
        -- pollUntilTrue requestLog 100000 $ \reqs -> any (findRequest (newKeys ^. #xo)) 
          pollUntilTrue requestLog 100000 $ 
             \reqs -> trace (show reqs) $ any (findRequest (newKeys ^. #xo)) reqs

    terminate 

  where 
    terminate = liftIO . putMVar shutdownTrigger $ ()

    -- let sinkForever = do
    --       sink GoBack
    --       liftIO $ putStrLn "Sent GoBack action"
    --       liftIO $ threadDelay 1000000  -- 1 second delay (in microseconds)
    --       sinkForever
    
    -- sinkForever

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

findRequest xo (Subscribe s) = 
  let filters = s ^. #filters
  in trace ("Filters are: " <> show filters) $ 
        any (relayListFilter xo) filters 
         && any (metadataFilter xo) filters
findRequest _ _ = False

relayListFilter xo f = 
  f ^. #kinds == Just [RelayList] && 
  f ^. #authors == Just [xo]

metadataFilter xo f = 
  f ^. #kinds == Just [Metadata] &&
  f ^. #authors == Just [xo]

timeoutTest :: String -> Int -> String -> JSM a -> ReaderT TestCtx JSM () 
timeoutTest testName i e a = do
  ctx <- ask 
  lift $ runTest ctx $ timeout i e a
 where 
  runTest :: TestCtx -> JSM (Either String a) -> JSM ()
  runTest TestCtx{..} test = do 
    let printLn = hPutStrLn hStdOut
        print = hPutStr hStdOut
    liftIO $ print $ "Running test " <> testName <> " ........ "
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
