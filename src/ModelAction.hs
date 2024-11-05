{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE FlexibleContexts #-}
-- {-# LANGUAGE TemplateHaskell #-}

module ModelAction where

import ContentUtils
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
import StoredRelay
import ProfilesLoader.Types
import Miso (JSM)
import Utils (Seconds)

data Action
  = RelayConnected RelayURI
  | PagedEventsProcess Bool (Lens' Model PagedEventsModel) Page [(Event, Relay)]
  | HandleWebSocket WebSocketAction
  | ReceivedProfiles [ProfOrRelays]
  | ReceivedReactions [(ReactionEvent, Relay)]
  | NoAction
  | StartAction
  | GoPage Page (Maybe ElementId)
  | Unfollow XOnlyPubKey
  | Follow XOnlyPubKey
  | WriteModel Model
  | ActualTime UTCTime
  | DisplayThread Event
  | DisplayReplyThread Event
  | GotReplyDraft Text
  | ThreadEvents [(Event, Relay)] Page
  | ProfileEvents [(Event, Relay)]
  | SubscribeForReplies [EventId]
  | SubscribeForParentsOf (Lens' Model PagedEventsModel) Page [Event]
  | FeedEventParentsProcess (Map.Map EventId (Set.Set EventId)) (Lens' Model PagedEventsModel) Page [(Event, Relay)]
  | SubscribeForEmbedded [EventId]
  | EmbeddedEventsProcess [(Event, Relay)]
  | GoBack
  | UpdateField (Lens' Model Text) Text                      -- TODO: see below
  | WriteTextToStorage Text Text
  | LoadProfile Bool XOnlyPubKey Page
  | SubState Page (SubscriptionId, SubState)
  | DisplayProfilePage (Maybe ElementId) XOnlyPubKey 
  | AddRelay
  | ShowFeed
  | ShowNext (Lens' Model PagedEventsModel) Page
  | ShowPrevious (Lens' Model PagedEventsModel)
  | LoadMoreEvents (Lens' Model PagedEventsModel) Page
  | LogConsole String
  | ScrollTo (Maybe Seconds) Text
  | SubscribeForEmbeddedReplies [EventId] Page
  | RepliesRecvNoEmbedLoading [(Event, Relay)]
  | ReportError Text
  | StartFeedLongRunning [DatedFilter]
  | FeedLongRunningProcess [(Event, Relay)]
  | ShowNewNotes
  | SendReplyTo Event (JSM Text)
  | ClearWritingReply
  | AllLoaded
  | SendUpdateProfile (JSM Profile)
  | ChangeRelayActive Text Bool
  | UpdatedRelaysList [StoredRelay]
  | RemoveRelay Text
  | Reload 
  | ListenToNotifs UTCTime
  | ProcessNewNotifs [(Event, Relay)]
  | ShowNotifications
  | SubscribeForPagedReactionsTo (Lens' Model PagedEventsModel) Page [ReactionEvent]
  | PagedReactionsToProcess (Lens' Model PagedEventsModel) Page [(Event, Relay)]
  | SendLike Event
  | LikeSent Event
  | DisplayMyProfilePage
 
data SubState = SubRunning (Map.Map Relay RelaySubState) | SubFinished (Map.Map Relay RelaySubState)
 deriving Eq

data Page
  = FeedPage
  | Following
  | ThreadPage Event
  | ProfilePage XOnlyPubKey
  | FindProfilePage
  | RelaysPage
  | MyProfilePage 
  | NotificationsPage 
  deriving (Show, Eq, Generic, Ord)

newtype ErrorCount = ErrorCount Int 
--  deriving newtype (Num, Eq)
  deriving Eq
newtype CloseCount = CloseCount Int 
--  deriving newtype (Num, Eq)
  deriving Eq

type ElementId = Text

data Model = Model
  { feed :: PagedEventsModel,
    feedNew :: [(Event, Relay)],
    notifs :: PagedEventsModel,
    notifsNew :: [(Event, Relay)],
    findWho :: Text,
    profileEvents :: Map.Map XOnlyPubKey PagedEventsModel,
    relaysPage :: RelaysPageModel,
    relaysList :: [StoredRelay],
    relaysStats :: Map.Map Text (Bool, ErrorCount, CloseCount), 
    reactions :: Reactions, -- TODO: what about deleted reactions?
    contacts :: Set.Set XOnlyPubKey,
    profiles :: Map.Map XOnlyPubKey (Profile, UTCTime),
    profileRelays :: Map.Map XOnlyPubKey ([Relay], UTCTime),
    page :: Page,
    now :: UTCTime, -- don't know a better way to supply time
    threads :: Map.Map RootEid Thread,
    writeReplyTo :: Maybe Event, -- event to reply to
    history :: [(Page, Maybe ElementId)], -- page and what element to scroll to
    subscriptions ::
      Map.Map
        Page
        [(SubscriptionId, SubState)],
    embedded :: Map EventId ((Event, [Content]), Set.Set Relay),
    errors :: [Text],
    fromRelays :: Map Event (Set.Set Relay),
    noteDraft :: Text,
    me :: XOnlyPubKey
  }
  deriving (Eq, Generic)

newtype CompactModel = CompactModel Model
-- Miso updates views whenever model changes. It uses Eq instance to determine that.
-- Here I define a custom one, so that updates don't happen unnecesarrily 
-- i.e. only data which is relevant for currently displayed page gets compared 
-- to determine if update is neccessary.
instance Eq CompactModel where
  (==) (CompactModel m1) (CompactModel m2) 
    | not . allEqual $ [eq #now, eq #subscriptions, eq #notifs, eq #notifsNew, eq #errors] = False
    | otherwise =
        if m1 ^. #page /= m2 ^. #page then m1 == m2 
        else 
          case m1 ^. #page of  
            -- TODO: would need heterogenous lists to get rid of eq 
            FeedPage -> allEqual $ [eq #feed, eq #feedNew, eq #profiles] ++ notesAndStuff
            Following -> allEqual [eq #profiles, eq #contacts]
            ThreadPage _ -> allEqual $ [eq #writeReplyTo, eq #noteDraft] ++ notesAndStuff
            ProfilePage xo -> allEqual $ [eq #contacts, eq #profiles, eq #profileRelays, eq (#profileEvents % at xo)] ++ notesAndStuff
            RelaysPage -> allEqual [eq #relaysStats, eq #relaysPage, eq #relaysList]
            MyProfilePage -> allEqual $ [eq $ #profiles % at (m1 ^. #me)]
            NotificationsPage -> allEqual $ [eq #notifs, eq #notifsNew] ++ notesAndStuff
            FindProfilePage -> allEqual $ [eq #findWho]
   where 
    eq ls = m1 ^. ls == m2 ^. ls
    allEqual = Prelude.all (==True) 
    notesAndStuff = [eq #threads, eq #reactions, eq #embedded]

newtype RootEid = RootEid EventId deriving (Eq, Ord)

newtype Since = Since UTCTime
  deriving (Eq)

newtype Until = Until UTCTime
  deriving (Eq)

type Threads = Map.Map RootEid Thread

data PagedEventsModel = PagedEventsModel
  { filter :: Maybe (Since -> Until -> [DatedFilter]),
    until :: Until,
    step :: NominalDiffTime,
    factor :: Integer,
    pg :: Int,
    pgStart :: Map.Map Int UTCTime, -- keep track of when each page starts
    pgSize :: Int,
    events :: [(Event, [Content])],
    fromRelays :: Map.Map EventId (Set.Set Relay), -- TODO:
    parents :: Map.Map EventId (Event, [Content]),
    -- events being reacted to by reactions in #events
    reactionEvents :: Map.Map EventId (Event, [Content]) 
  }
  deriving (Generic)

defaultPagedModel :: Until ->
  PagedEventsModel
defaultPagedModel until@(Until t) =
  PagedEventsModel
    { filter = Nothing,
      until = until,
      step = nominalDay / 2,
      factor = 1,
      pg = 0,
      pgStart = Map.fromList [(0, t)],
      pgSize = 15,
      events = [],
      fromRelays = Map.empty,
      parents = Map.empty,
      reactionEvents = Map.empty
    }

defaultProfilesModel :: XOnlyPubKey -> UTCTime -> PagedEventsModel
defaultProfilesModel xo now  = 
  defaultPagedModel (Until now)
      & #filter .~ Just (pagedFilter [xo])
      & #factor .~ 2
      & #step .~ 5 * nominalDay
 where 
  pagedFilter xos =
    \(Since s) (Until u) ->
      textNotesWithDeletes
      (Just s)
      (Just u)
      xos

-- TODO: alter this
instance Eq PagedEventsModel where
  f1 == f2 =
    f1 ^. #pg
      == f2 ^. #pg
      && f1 ^. #pgSize == f2 ^. #pgSize
      && f1 ^. #step == f2 ^. #step
      && Prelude.length (f1 ^. #events) == Prelude.length (f2 ^. #events)
      && f1 ^. #parents == f2 ^. #parents
      && f1 ^. #reactionEvents == f2 ^. #reactionEvents

data RelayState = Connected | Disconnected | Error

data RelaysPageModel = RelaysPageModel
  { relay :: Text
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
    events :: ProcessedEvents,
    -- keep track of what events you already proccessed
    -- and where are they all from
    fromRelays :: Map.Map EventId (Set.Set Relay)
  }
  deriving (Eq, Generic)

newThread :: Thread
newThread = Thread Map.empty Map.empty Map.empty Map.empty

type ProcessedEvents = Map.Map EventId (Event, [Content])

addToThread :: (Event, [Content]) -> Thread -> Thread
addToThread ec@(e, _) t =
  let replyToEid = findIsReplyTo e
   in case replyToEid of
        Just eid ->
          t
            & #events
            % at (e ^. #eventId)
            ?~ ec
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
        Nothing -> -- it's a root event if it's not a reply to anything.
          t
            & #events
            % at (e ^. #eventId)
            ?~ ec

getRepliesFor :: Thread -> EventId -> [(Event, [Content])]
getRepliesFor t eid = fromMaybe [] $
  do
    replIds <- Set.toList <$> t ^. #replies % at eid
    let replies = catMaybes $ (\r -> Map.lookup r (t ^. #events)) <$> replIds
    pure replies


-- makePrisms ''Since