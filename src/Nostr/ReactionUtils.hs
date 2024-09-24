module Nostr.ReactionUtils where

import Nostr.Event
import Data.List (unsnoc)

reactionToEvent :: Event -> Maybe EventId
reactionToEvent e = do
  (ETag eid _ _) <- snd <$> (unsnoc . filter isETag . tags $ e)
  pure eid

isETag :: Tag -> Bool
isETag (ETag _ _ _) = True
isETag _ = False

