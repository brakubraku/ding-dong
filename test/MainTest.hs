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
  Test.runTest
#endif