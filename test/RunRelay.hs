module RunRelay where 

import Relay.RelayServer
import Relay.Database
import Control.Concurrent
import qualified Data.Sequence as Seq

main :: IO ()
main = do 
  mdb <- newMVar emptyDB
  requestLog <- newMVar Seq.empty
  runRelay mdb requestLog 8080