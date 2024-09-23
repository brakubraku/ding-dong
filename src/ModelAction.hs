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
import ContentUtils

data Action
  = RelayConnected RelayURI
  | PagedNotesProcess (Lens' Model PagedNotesModel)  [(Event, Relay)]
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
  | SubscribeForEmbedded [EventId]
  | EmbeddedEventsProcess [(Event, Relay)]
  | GoBack
  | UpdateField (Lens' Model Text) Text
  | FindProfile
  | SubState Page (SubscriptionId, Map.Map Relay RelaySubState)
  | DisplayProfilePage XOnlyPubKey
  | LogReceived [(Event, Relay)]
  | AddRelay
  | ShowFeed
  | ShowNext (Lens' Model PagedNotesModel) Page
  | ShowPrevious (Lens' Model PagedNotesModel) 
  | LoadMoreNotes (Lens' Model PagedNotesModel) Page
  | LogConsole String

data Page
  = FeedPage
  | Following
  | ThreadPage Event
  | ProfilePage
  | RelaysPage
  deriving (Show, Eq, Generic, Ord)

data Model = Model
  { feed :: PagedNotesModel,
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
    relays :: [Text],
    embedded :: Map EventId ((Event, [Content]), Set.Set Relay)
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

data PagedNotesModel = PagedNotesModel
  { 
    filter :: Maybe (Since -> Until -> [DatedFilter]),
    since :: Maybe Since,
    step :: NominalDiffTime,
    page :: Int,
    pageSize :: Int,
    notes :: [(Event, [Content])]
  }
  deriving (Generic)

defaultPagedModel ::
  PagedNotesModel
defaultPagedModel =
  PagedNotesModel
    { 
      filter = Nothing,
      since = Nothing,
      step = nominalDay / 2,
      page = 0,
      pageSize = 30,
      notes = [] 
    }
    
-- TODO: alter this
instance Eq PagedNotesModel where
  f1 == f2 =
    f1 ^. #page
      == f2 ^. #page
      && f1 ^. #pageSize == f2 ^. #pageSize
      && f1 ^. #step == f2 ^. #step

data RelayState = Connected | Disconnected | Error

data RelaysPageModel = RelaysPageModel
  { relay :: Text
  }
  deriving (Eq, Generic)

data FindProfileModel = FindProfileModel
  { findWho :: Text,
    lookingFor :: Maybe XOnlyPubKey,
    events :: PagedNotesModel
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

type EventsWithRelays = Map.Map EventId ((Event, [Content]), Set.Set Relay)

addEvent :: ((Event, [Content]), Relay) -> EventsWithRelays -> EventsWithRelays -- TODO: remove deleted events
addEvent ((evt, c), rel) ers =
  ers & at (evt ^. #eventId) %~ \x -> Just $
    case x of
      Just (ec, rs) -> (ec, Set.insert rel rs)
      Nothing -> ((evt, processContent evt), Set.singleton rel)

addToThread :: ((Event, [Content]), Relay) -> Thread -> Thread
addToThread ((e,c), rel) t =
  let replyToEid = findIsReplyTo e
   in case replyToEid of
        Just eid ->
          t
            & #events
            %~ addEvent ((e,c), rel)
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

getRepliesFor :: Thread -> EventId -> [(Event, [Content])]
getRepliesFor t eid = fromMaybe [] $
  do
    replIds <- Set.toList <$> t ^. #replies % at eid
    let replies = catMaybes $ (\r -> Map.lookup r (t ^. #events)) <$> replIds
    pure $ fst <$> replies
