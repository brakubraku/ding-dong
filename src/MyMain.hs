{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module MyMain where

import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Aeson (ToJSON)
import Data.Bool
import Data.DateTime (DateTime)
import Data.Either (fromRight)
import Data.List
import Data.Map as Map
import qualified Data.Map as M
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Time
import Debug.Trace (trace)
import GHC.Generics
import Miso hiding (at, now, send)
import Miso.String (MisoString)
import qualified Miso.String as S
import MisoSubscribe (subscribe)
import ModelAction
import MyCrypto
import Nostr.Event
import Nostr.Filter
import Nostr.Keys
import Nostr.Kind
import Nostr.Network
import Nostr.Profile
import Nostr.Reaction
import Nostr.Relay
import qualified Nostr.RelayPool as RP
import Nostr.Request
import Nostr.Response
import Nostr.WebSocket
import Optics as O
import PeriodicLoader
import ReactionsLoader (createReactionsLoader)

start :: JSM ()
start = do
  keys <- loadKeys
  savedContacts <- loadContacts
  actualTime <- liftIO getCurrentTime
  liftIO . putStrLn $ "Keys are:" <> show keys
  nn <-
    liftIO $
      initNetwork
        [ "wss://relay.nostrdice.com",
          "wss://lunchbox.sandwich.farm",
          "wss://relay.nostr.net",
          "wss://polnostr.xyz",
          "wss://nostr.at"
        ]
        keys
  reactionsLoader <- liftIO createReactionsLoader
  let subs = [connectRelays nn HandleWebSocket]
      update = updateModel nn reactionsLoader
  startApp App {initialAction = StartAction, model = initialModel savedContacts actualTime, ..}
  where
    initialModel contacts actualTime =
      Model
        Set.empty
        (Reactions Map.empty Map.empty)
        "No error yet bitch!"
        contacts
        Map.empty
        Home
        actualTime
        (Thread Map.empty Map.empty Map.empty)
    events = defaultEvents
    view = appView
    mountPoint = Nothing
    logLevel = Off

updateModel ::
  NostrNetwork ->
  PeriodicLoader EventId (ReactionEvent, Relay) ->
  Action ->
  Model ->
  Effect Action Model
updateModel nn rl action model =
  case action of
    HandleWebSocket (WebSocketClose _ _ _) ->
      noEff $ model & #err .~ "Connection closed"
    HandleWebSocket (WebSocketError er) ->
      noEff $ model & #err .~ er
    -- HandleWebSocket WebSocketOpen ->
    StartAction ->
      do
        let simpleF f = DatedFilter f Nothing Nothing
            textNotes = simpleF $ TextNoteFilter $ model ^. #contacts
            -- getContacts = simpleF $ ContactsFilter
            getProfiles = simpleF $ MetadataFilter $ model ^. #contacts

        effectSub
          model
          ( \sink ->
              do
                let runInNostr = liftIO . flip runReaderT nn
                -- wait for connections to relays having been established
                runInNostr $ RP.waitForActiveConnections (secs 2)
                -- TODO: Is this the way to run 2 subs in parallel?
                forkJSM $ startLoader nn rl ReceivedReactions sink
                forkJSM $ subscribe nn [textNotes] TextNotesAndDeletes Right sink
                forkJSM $ subscribe nn [getProfiles] ReceivedProfiles Right sink
                forkJSM $ -- put actual time to model every 60 seconds
                  ( \sink ->
                      do
                        now <- liftIO getCurrentTime
                        liftIO . sink . ActualTime $ now
                        liftIO . threadDelay . secs $ 60
                  )
                    sink
                    -- runInNostr $ saveContacts $ zip (model ^. #contacts) (repeat Nothing)
          )
    TextNotesAndDeletes rs ->
      -- TODO: test this
      let notes = model ^. #textNotes
          newEvents = catMaybes $ getEvent . fst <$> rs
          newNotes =
            Set.fromList $
              Prelude.filter
                (\e -> e ^. #kind == TextNote && not (isReply e))
                newEvents
          allNotes = notes `Set.union` newNotes
          deletionEvents =
            catMaybes $
              Prelude.filter ((== Delete) . kind) newEvents
                & fmap
                  ( \e -> do
                      ETag eid _ _ <- getSingleETag e
                      pure (e ^. #pubKey, eid)
                  )
          updatedNotes =
            Set.filter
              ( \e ->
                  not $
                    (e ^. #pubKey, e ^. #eventId)
                      `elem` deletionEvents
              )
              allNotes

          newModel =
            model
              & #textNotes
              .~ updatedNotes
          reactionsToLoadFor =
            (eventId <$> Set.toList (newNotes `Set.difference` notes))
       in newModel
            <# (load rl reactionsToLoadFor >> pure NoAction)
    ReceivedReactions rs ->
      -- traceM "Got reactions my boy"
      let reactions = model ^. #reactions
       in trace ("  " <> show (length rs)) $
            noEff $
              model
                & #reactions
                .~ Prelude.foldl processReceived reactions rs
    ReceivedProfiles rs ->
      let profiles = catMaybes $ extractProfile <$> rs
       in noEff $
            model
              & #profiles
              %~ unionWithKey
                ( \_ p1@(_, d1) p2@(_, d2) ->
                    -- prefer most recent profile
                    if d1 > d2 then p1 else p2
                )
                (fromList profiles)
    GoPage page ->
      noEff $ model & #page .~ page
    Unfollow xo ->
      let updated = Prelude.filter (/= xo) $ model ^. #contacts
       in (model & #contacts .~ updated)
            <# (updateContacts updated >> pure NoAction)
    WriteModel m ->
      model <# (writeModelToStorage m >> pure NoAction)
    ActualTime t -> noEff $ model & #now .~ t
    DisplayThread e -> do
      effectSub (model & #page .~ ThreadPage e) $ subscribeForThreadEvents nn e
    ThreadEvents rootEvtId rs -> do
      let evts =
            catMaybes $
              ( \(resp, rel) -> do
                  evt <- getEvent resp
                  pure (evt, rel)
              )
                <$> rs
      let newModel = model & #thread.~ Prelude.foldr addToThread (model ^. #thread) evts
      newModel <# (load rl (Map.keys $ newModel ^. #thread % #events) >> pure NoAction)

    _ -> noEff model

subscribeForThreadEvents :: NostrNetwork -> Event -> Sub Action
subscribeForThreadEvents nn e sink = do
  -- if there is no root eid in tags then this is a "top-level" note
  -- and so it's eid is the root of the thread
  let rootEvtId = fromMaybe (e ^. #eventId) $ findRootEid e
  subscribe
    nn
    [anytime $ LinkedEvents [rootEvtId]]
    (ThreadEvents rootEvtId)
    Right
    sink

writeModelToStorage :: Model -> JSM ()
writeModelToStorage m = pure ()

updateContacts :: [XOnlyPubKey] -> JSM ()
updateContacts xos = do
  setLocalStorage "my-contacts" $ xos

loadContacts :: JSM [XOnlyPubKey]
loadContacts = fromRight [] <$> getLocalStorage "my-contacts"

extractProfile ::
  (Response, Relay) ->
  Maybe (XOnlyPubKey, (Profile, DateTime))
extractProfile (EventReceived _ event, _) = parseProfiles event
  where
    parseProfiles e =
      let xo = pubKey e
       in case readProfile e of
            Just p -> Just (xo, (p, created_at e))
            Nothing -> Nothing
extractProfile _ = Nothing

secs :: Int -> Int
secs = (* 1000000)

appView :: Model -> View Action
appView m =
  div_ [] $
    [ -- h1_
      --   [style_ $ M.fromList [("font-weight", "bold")]]
      --   [text $ S.pack "ding-dong"],
      --
      --
      -- ,input_
      --   [ type_ "text"
      --   --  , onInput UpdateMessage
      --   -- onEnter (SendMessage msg)
      --   ],
      -- button_
      --   []
      --   -- [ onClick (SendMessage msg)    ]
      --   [text (S.pack "Do nothing!")]

      div_
        [class_ "main-container"]
        [ leftPanel,
          rightPanel m
        ],
      footerView m
    ]

followingView :: Model -> View Action
followingView m@Model {..} =
  div_
    [class_ "following-container"]
    $ displayProfile
      <$> loadedProfiles
  where
    loadedProfiles :: [(XOnlyPubKey, Profile)]
    loadedProfiles =
      catMaybes $
        ( \xo -> do
            p <- fst <$> (m ^. #profiles % at xo)
            pure (xo, p)
        )
          <$> contacts

    displayProfile :: (XOnlyPubKey, Profile) -> View Action
    displayProfile (xo, p) =
      div_
        [class_ "profile"]
        [ button_
            [class_ "profile-unfollow-button", onClick (Unfollow xo)]
            [text "Unfollow"],
          div_
            [class_ "profile-pic-container"]
            [displayProfilePic $ p ^. #picture],
          div_
            [class_ "profile-info-container"]
            [ div_ [class_ "profile-name"] [text $ p ^. #username],
              div_ [class_ "profile-about"] [text . fromMaybe "" $ p ^. #about]
            ]
        ]

notesView :: Model -> View Action
notesView m@Model {..} =
  div_
    [class_ "notes-container"]
    (displayNote m <$> (Set.toList textNotes))

footerView :: Model -> View action
footerView Model {..} =
  div_
    [class_ "footer"]
    [ p_
        [style_ $ M.fromList [("font-weight", "bold")]]
        [text err | not . S.null $ err]
    ]

displayProfilePic :: Maybe Picture -> View action
displayProfilePic (Just pic) =
  img_
    [ class_ "profile-pic",
      prop "src" $ pic
    ]
displayProfilePic _ = div_ [class_ "profile-pic"] []

displayNote :: Model -> Event -> View Action
displayNote m e =
  div_
    [class_ "note-container"]
    [ div_
        [class_ "profile-pic-container"]
        [displayProfilePic $ picUrl m e],
      div_
        [class_ "text-note-container", onClick $ DisplayThread e]
        [ div_ [class_ "profile-info"] [profileName, displayName, noteAge],
          div_
            [class_ "text-note"]
            [ p_ [] [text (e ^. #content)],
              reactionsView reactions
            ]
        ],
      div_ [class_ "text-note-right-panel"] []
    ]
  where
    profile = fromMaybe unknown $ getAuthorProfile m e
    unknown = Profile {username = "", displayName = Nothing}
    profileName :: View action
    profileName = span_ [id_ "username"] [text $ profile ^. #username]
    displayName :: View action
    displayName = span_ [id_ "display-name"] [text . fromMaybe "" $ profile ^. #displayName]
    noteAge = span_ [id_ "note-age"] [text . S.pack $ displayAge (m ^. #now) e]
    reactions = m ^. #reactions % #processed % at (eventId e)

-- >>> length (Just "a")
-- 1

rightPanel :: Model -> View Action
rightPanel m =
  div_
    [class_ "right-panel"]
    [displayPage]
  where
    displayPage = case m ^. #page of
      Home -> notesView m
      Following -> followingView m
      ThreadPage e -> displayThread m e

leftPanel :: View Action
leftPanel =
  div_
    [class_ "left-panel"]
    [ spItem "Feed" Home,
      spItem "Notifications" Following,
      spItem "Following" Following,
      spItem "Followers" Following,
      spItem "Bookmarks" Following
    ]
  where
    spItem label page =
      div_
        [class_ "left-panel-item"]
        [button_ [onClick (GoPage page)] [text label]]

displayThread :: Model -> Event -> View Action
displayThread m e =
  let thread = m ^. #thread
      parentId = thread ^. #parents % at (e ^. #eventId)
      parent = parentId >>= \eid -> thread ^. #events % at eid
      parentDisplay =
        parent
          >>= (\p -> Just $ div_ [class_ "parent"] [displayNote m (fst p)])
      noteDisplay =
        Just $
          div_
            [ class_ $
                if isJust parent
                  then "note"
                  else "note-no-parent"
            ]
            [displayNote m e]
      replies = getRepliesFor (m ^. #thread) (e ^. #eventId)
      repliesDisplay =
        (\r -> (div_ [class_ "reply"] [displayNote m r])) <$> replies
   in div_ [class_ "thread-container"] $
        catMaybes [parentDisplay, noteDisplay] ++ repliesDisplay

reactionsView :: Maybe (Map Sentiment (Set.Set XOnlyPubKey)) -> View action
reactionsView Nothing = div_ [class_ "reactions-container"] [text ("")]
reactionsView (Just reactions) =
  let likes = "♥ " <> (S.pack . show . length . fromMaybe Set.empty $ (reactions ^. at Like))
      dislikes = "🖓 " <> (S.pack . show . length . fromMaybe Set.empty $ (reactions ^. at Dislike))
      others = "Others: " <> (S.pack . show . length . fromMaybe Set.empty $ (reactions ^. at Other))
   in div_ [class_ "reactions-container"] [text (likes <> " " <> dislikes <> " " <> others)]

getAuthorProfile :: Model -> Event -> Maybe Profile
getAuthorProfile m e = fst <$> m ^. #profiles % at (e ^. #pubKey)

picUrl :: Model -> Event -> Maybe MisoString
picUrl m e = do
  Profile {..} <- getAuthorProfile m e
  picture

loadKeys :: JSM Keys
loadKeys = do
  let identifier = "my-keys"
  keys <- getLocalStorage identifier
  case keys of
    Right k -> pure k
    Left _ -> do
      newKeys <- liftIO $ generateKeys
      setLocalStorage identifier newKeys
      pure newKeys

displayAge :: UTCTime -> Event -> String
displayAge now e =
  let ageSeconds =
        round $ diffUTCTime now (e ^. #created_at)
      format :: Integer -> String
      format s
        | days > 0 = show days <> "d"
        | hours > 0 = show hours <> "h"
        | minutes > 0 = show minutes <> "m"
        | otherwise = ""
        where
          days = s `div` (3600 * 24)
          hours = s `div` 3600
          minutes = s `div` 60
   in format ageSeconds
