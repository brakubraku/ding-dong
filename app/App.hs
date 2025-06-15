{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE ForeignFunctionInterface       #-}

module App where 

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
#else 
import Language.Javascript.JSaddle.Types
import Language.Javascript.JSaddle.Warp as JSaddle.Warp
#endif

import qualified Miso.Run
import qualified DingDong

#ifdef wasi_HOST_OS

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run DingDong.start 

#else   
main :: IO ()
-- main = JSaddle.Warp.run 1234 $ DingDong.start
main = Miso.Run.run DingDong.start
#endif  
