{-# LANGUAGE CPP                        #-}
{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE ForeignFunctionInterface       #-}

module App where 

#ifdef wasi_HOST_OS
import GHC.Wasm.Prim
import qualified Language.Javascript.JSaddle.Wasm as JSaddle.Wasm
#else 
import Language.Javascript.JSaddle.Types
#endif

import qualified DingDong

#ifdef wasi_HOST_OS

foreign export javascript "hs_start" main :: JSString -> IO ()

main :: JSString -> IO ()
main e = JSaddle.Wasm.run DingDong.start 

#else   
main ::  IO ()
main = pure ()
#endif  
