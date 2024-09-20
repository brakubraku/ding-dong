{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}

module ModelAction where

import Data.DateTime (DateTime)
import Data.Map as Map
import Data.Maybe
import qualified Data.Set as Set
import Data.Time
import GHC.Generics
import Miso.String
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Network
import Nostr.Profile
import Nostr.Reaction
import Nostr.Relay
import Nostr.Request
import Nostr.WebSocket
import Optics

data Action
  = RelayConnected RelayURI
  | FeedNotesProcess [(Event, Relay)]
  | HandleWebSocket (WebSocket ())
  | ReceivedProfiles [(XOnlyPubKey, Profile, DateTime, Relay)]
  | ReceivedReactions [(ReactionEvent, Relay)]
  | NoAction
  | StartAction
  | GoPage Page
  | Unfollow XOnlyPubKey
  | Follow XOnlyPubKey
  | WriteModel Model
  | ActualTime UTCTime
  | DisplayThread Event
  | ThreadEvents [(Event, Relay)]
  | ProfileEvents [(Event, Relay)]
  | SubscribeForReplies [Event]
  | GoBack
  | UpdateField (Lens' Model Text) Text
  | FindProfile
  | SubState Page (SubscriptionId, Map.Map Relay RelaySubState)
  | DisplayProfilePage XOnlyPubKey
  | LogReceived [(Event, Relay)]
  | AddRelay
  | ShowFeed
  | ShowMore
  | LoadMore

data Page
  = FeedPage
  | Following
  | ThreadPage Event
  | ProfilePage
  | RelaysPage
  deriving (Show, Eq, Generic, Ord)

data Model = Model
  { feed :: FeedPageModel,
    fpm :: FindProfileModel,
    relaysPage :: RelaysPageModel,
    reactions :: Reactions, -- TODO: what about deleted reactions?
    err :: MisoString,
    contacts :: Set.Set XOnlyPubKey,
    profiles :: Map.Map XOnlyPubKey (Profile, DateTime),
    page :: Page,
    now :: UTCTime, -- don't know a better way to supply time
    threads :: Map.Map RootEid Thread,
    history :: [Page],
    subscriptions ::
      Map.Map
        Page
        [(SubscriptionId, Map.Map Relay RelaySubState)],
    relays :: [Text]
  }
  deriving (Eq, Generic)

-- TODO: Declare your own Eq Model instance depending on
--       *when* you want Miso update function to trigger!

-- perhaps Eq instance is not appropriate for this
-- TODO: modify Miso so that there is a typeclass
-- class TriggerViewUpdate a where
--      shouldTrigger :: a -> a -> Bool -- oldModel -> newModel -> Bool
-- and Model must be instance of it

-- instance Eq Model where
--   m1 == m2 =
--     case (m1 ^. #page) of
--       Feed ->

newtype RootEid = RootEid EventId deriving (Eq, Ord)

newtype Since = Since DateTime
  deriving (Eq)

newtype Until = Until DateTime
  deriving (Eq)

type Threads = Map.Map RootEid Thread

data FeedPageModel = FeedPageModel
  { filter :: Since -> Until -> [DatedFilter],
    since :: Since,
    until :: Until,
    step :: Maybe NominalDiffTime,
    page :: Int,
    pageSize :: Int,
    notes :: [Event]
  }
  deriving (Generic)

defaultFeedPageModel ::
  Set.Set XOnlyPubKey ->
  Since ->
  Until ->
  FeedPageModel
defaultFeedPageModel xos since until =
  FeedPageModel
    { filter =
        \(Since s) (Until u) ->
          textNotesWithDeletes (Just s) (Just u) $ Set.toList xos,
      since = since,
      until = until,
      step = Just $ nominalDay/2,
      page = 0,
      pageSize = 30,
      notes = []
    }

-- TODO: don't really care about it
instance Eq FeedPageModel where
  f1 == f2 = True

data RelayState = Connected | Disconnected | Error

data RelaysPageModel = RelaysPageModel
  { relay :: Text
  }
  deriving (Eq, Generic)

data FindProfileModel = FindProfileModel
  { findWho :: Text,
    lookingFor :: Maybe XOnlyPubKey,
    events :: Map Event (Set.Set Relay)
  }
  deriving (Eq, Generic)

data Thread = Thread
  { -- Mapping from event to it's replies
    replies :: Map.Map EventId (Set.Set EventId),
    -- Mapping from event to that event which it's a reply to
    parents :: Map.Map EventId EventId,
    -- We are mapping to EventId-s above
    -- (as opposed mapping directly to Event-s) because those
    -- events may not be loaded yet.
    -- Loaded events are stored in 'events' below
    events :: EventsWithRelays
  }
  deriving (Eq, Generic)

newThread :: Thread
newThread = Thread Map.empty Map.empty Map.empty

type EventsWithRelays = Map.Map EventId (Event, Set.Set Relay)

addEvent :: (Event, Relay) -> EventsWithRelays -> EventsWithRelays -- TODO: remove deleted events
addEvent (evt, rel) ers =
  ers & at (evt ^. #eventId) %~ \x -> Just $
    case x of
      Just (e, rs) -> (e, Set.insert rel rs)
      Nothing -> (evt, Set.singleton rel)

addToThread :: (Event, Relay) -> Thread -> Thread
addToThread (e, rel) t =
  let replyToEid = findIsReplyTo e
   in case replyToEid of
        Just eid ->
          t
            & #events
            %~ addEvent (e, rel)
            & #parents
            % at (e ^. #eventId)
            .~ Just eid
            & #replies
            % at eid
            %~ \mset -> Just $
              case mset of
                Just set ->
                  Set.insert (e ^. #eventId) set
                Nothing ->
                  Set.singleton (e ^. #eventId)
        Nothing -> t

getRepliesFor :: Thread -> EventId -> [Event]
getRepliesFor t eid = fromMaybe [] $
  do
    replIds <- Set.toList <$> t ^. #replies % at eid
    let replies = catMaybes $ (\r -> Map.lookup r (t ^. #events)) <$> replIds
    pure $ fst <$> replies
