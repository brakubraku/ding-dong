{-# LANGUAGE CPP                        #-}
-- {-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE ForeignFunctionInterface       #-}
module MainTest where 

import qualified Test
import Relay.RelayServer
import Relay.Database

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as Wasm
#else 
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Warp as Warp
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar, killThread, ThreadId, readMVar)
import Control.Exception (bracket_, try, SomeException)
import System.Process (createProcess, proc, waitForProcess, CreateProcess(..))
import System.Exit (exitSuccess)
import qualified Data.Map as Map
#endif

#ifdef wasi_HOST_OS

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = Wasm.run Test.test

#else   
main ::  IO ()
main = do
  shutdownSignal <- newEmptyMVar
  -- Start headless client in a separate thread and save its ThreadId
  headlessTid <- forkIO $ do
    putStrLn "Waiting 3 seconds for jsaddle-warp to fully start..."
    threadDelay 3000000  -- Wait 3 seconds for jsaddle-warp to start
    -- To request shutdown from this thread: putMVar shutdownSignal ()
    Test.runHeadlessClient True Map.empty
  -- You can add more child threads and save their ThreadIds in a list
  -- Main thread waits for shutdown signal
  forkIO $ Test.runTest shutdownSignal
  readMVar shutdownSignal
  -- On shutdown, kill all child threads and exit
  putStrLn "Shutting down all threads."
  killThread headlessTid
  -- kill other threads if needed
  exitSuccess
#endif