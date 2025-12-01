{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedLabels #-}

module Nostr.PagedChronologicalEvents where

import Data.Time (NominalDiffTime, UTCTime, addUTCTime, nominalDay)
import Nostr.Filter (DatedFilter)
import GHC.Num (Natural)
import qualified Data.Map as Map
import Nostr.Event (EventId, Event)
import qualified Data.Set as Set
import Nostr.Relay (Relay)
import ContentUtils
import GHC.Generics (Generic)
import Prelude hiding (until, filter)
import Nostr.Network (NostrNetworkT)
import Optics
-- import ModelAction hiding (filter, Until, Since)
import Miso (Effect)
import MisoSubscribe (SubscriptionParams)
import Data.Maybe (fromMaybe)

data Since = Since UTCTime -- | NostrEarliest -- represent the earliest possible time
  deriving (Eq)

newtype Until = Until UTCTime
  deriving (Eq)

-- TODO: decouple this from Miso and create PagedChronologicalEvents type
data PagedChronologicalEvents a = PagedChronologicalEvents
  { currentPage :: Natural,
    -- how many events per page. if you receive more, then move them to the next page
    pgSize :: Natural,
    -- keep track of date range for each page
    -- previous page Until needs to be next page's Since
    pages :: Map.Map Natural (Since, Until),
    events :: [a],
    -- Nostr filter to get events since and until some date
    filter :: Since -> Until -> [DatedFilter],
    -- how much to step back in time
    step :: NominalDiffTime,
    -- by what factor to increase step, when there are not enough events for current page
    factor :: Integer,
    -- keep track from which relay events were received
    fromRelays :: Map.Map EventId (Set.Set Relay), -- TODO:
    -- parents of events in #events
    parents :: Map.Map EventId (Event, [Content]),
    -- events being reacted to by reactions in #events
    reactionEvents :: Map.Map EventId (Event, [Content]),
    -- how to process received events for this paged model 
    -- TODO: decouple from Miso
    process :: [(Event, Relay)] -> [a], 
    -- TODO: move this to a typeclass?
    getEvent :: a -> Event
  }
  deriving (Generic)

-- processReceived :: PagedChronologicalEvents a -> [(Event, Relay)] -> PagedChronologicalEvents a
-- processReceived pcm received =
--   let newEvents = pcm.process received

defaultPagedModel :: Until ->
  (PagedChronologicalEvents a)
defaultPagedModel (Until until) =
  PagedChronologicalEvents
    { filter = const $ const [],
      step = step,
      factor = 1,
      currentPage = 0,
      pages = Map.fromList [(0, (Since (addUTCTime (-step) until), Until until))],
      pgSize = 15,
      events = [],
      fromRelays = Map.empty,
      parents = Map.empty,
      reactionEvents = Map.empty,
      process = error "Unitiliazed process function",
      getEvent = error "Unitiliazed getEvent function"
    }
  where step  = nominalDay / 6

-- loadEvents :: 
--  (SubscriptionParams -> Effect Model Action) -> 
--   Lens' Model (PagedChronologicalEvents a) -> 
--   UIPage -> 
--   Effect Model Action
-- loadEvents subcribe pel page = do
--   model <- get
--   let pe = model ^. pel
--       currentPage = model ^. pel % #currentPage
--       factoredStepFrom when = addUTCTime (pe ^. #step * (-fromInteger (pe ^. #factor))) when
--       (previousPageSince, _) = pe % #pages ^. at (currentPage - 1)
--   (since, until) <-         
--     case pe ^. #pages ^. at currentPage of
--       Nothing -> do
--         let since = Since $ factoredStepFrom previousPageSince
--             until = Until previousPageSince
--         put $ model & pel % #pages % at currentPage ?~ (since, until)
--         pure (since, until)
--       Just (Since sinceOld, until) -> do
--         let since = Since $ factoredStepFrom sinceOld
--         put $ model & pel % #pages % at currentPage ?~ (since, until)
--         pure (since, until)
--   subscribe $
--     allAtEOSOnPage
--       page
--       (filter since until)
--       (model ^. pml % #process $ page)
      
nostrEarliest :: UTCTime
nostrEarliest = read "2020-01-01 00:00:00 UTC" :: UTCTime

loadEvents :: 
  PagedChronologicalEvents a -> 
  (PagedChronologicalEvents a, [DatedFilter])
loadEvents pe = 
  let currentPage = pe ^. #currentPage
      factoredStepFrom when = addUTCTime (pe ^. #step * (-fromInteger (pe ^. #factor))) when
  in  
    case pe ^. #pages % at (currentPage - 1) of 
      Nothing -> (pe, [])
      Just (Since previousPageSince, _) -> 
        let (updated, since, until) =         
              case pe ^. #pages % at currentPage of
                Nothing ->
                  let since = Since $ factoredStepFrom previousPageSince
                      until = Until $ previousPageSince
                  in (pe & #pages % at currentPage ?~ (since, until), since, until)
                Just (Since sinceOld, until) ->
                  let since = Since $ factoredStepFrom sinceOld
                  in (pe & #pages % at currentPage ?~ (since, until), since, until)
        in (updated, (pe ^. #filter) since until)

-- show next page
showNext :: 
   PagedChronologicalEvents a -> 
  (PagedChronologicalEvents a, [DatedFilter])
showNext pe = 
  loadEvents $ pe & #currentPage %~ (+1)


-- process received events


-- increase factor if still not enough messages on page
-- processEvents :: PagedChronologicalEvents a -> [(Event, Relay)] -> PagedChronologicalEvents a
-- processEvents pe received = do
