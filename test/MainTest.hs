{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module MainTest where 

import Test
import TestEvents
import Optics
import Control.Monad.Reader
import qualified Data.Map as Map

import Nostr.Keys

runTest :: IO ()
runTest = do
  newKeys <- generateKeys
  withDingDong newKeys [simpleContacts newKeys] Map.empty $ do
      TestContext{..} <- ask
      timeoutTest 
        "See if metadata and relaylist is requested" 
        1000000 
        "Did not find metadata and relaymetadata requests" $ 
          pollUntilTrue requestLog 100000 $ 
             any (findRequest (newKeys ^. #xo))
  
main ::  IO ()
main = runTest