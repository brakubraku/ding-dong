{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- optics support
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RecordWildCards #-}

module Nostr.Network where

import Control.Concurrent
import Data.Bifunctor (Bifunctor (second))
import Data.Map
import GHC.Generics
import Miso.FFI.WebSocket
import Miso.String hiding (all)
import Optics

type Relay = MisoString

type RelayConnections = Map Relay (Socket, Bool)

data NostrNet = NostrNet
  { conns :: MVar RelayConnections -- map from relay name to socket and whether it's connected
  }
  deriving (Generic)

allConnected :: NostrNet -> IO Bool
allConnected NostrNet {..} = do
  rcs <- readMVar conns
  pure . all snd $ (elems rcs)

markConnected :: Relay -> NostrNet -> IO ()
markConnected r NostrNet {..} = do
  modifyMVar_ conns $ \cs -> do
    pure $ cs & at r %~ fmap (second (const True))

-- insert