{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}

module MyMain where

import BechUtils
import Control.Concurrent
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bool (bool)
import Data.DateTime (DateTime)
import Data.Either (fromRight)
import Data.Map as Map hiding (filter, foldr, singleton)
import qualified Data.Map as M hiding (filter, foldr, singleton)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import qualified Data.Set as Set
import Data.Time
import Debug.Trace (trace)
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
import Nostr.Response
import Nostr.WebSocket
import Optics as O
import PeriodicLoader
import ProfilesLoader
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
  profilesLoader <- liftIO createProfilesLoader
  let subs = [connectRelays nn HandleWebSocket]
      update = updateModel nn reactionsLoader profilesLoader
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
        Map.empty
        [Home]
        (FindProfileModel "" Nothing Map.empty)
        Map.empty
    events = defaultEvents
    view = appView
    mountPoint = Nothing
    logLevel = Off

updateModel ::
  NostrNetwork ->
  PeriodicLoader EventId (ReactionEvent, Relay) ->
  PeriodicLoader XOnlyPubKey (XOnlyPubKey, Profile, DateTime, Relay) ->
  Action ->
  Model ->
  Effect Action Model
updateModel nn rl pl action model =
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
                forkJSM $ startLoader nn pl ReceivedProfiles sink
                forkJSM $
                  subscribe
                    nn
                    [textNotes]
                    TextNotesAndDeletes
                    (Just $ SubState Home)
                    Right
                    sink
                -- forkJSM $ subscribe nn [getProfiles] ReceivedProfiles Right sink
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
      -- TODO: do this using List partition
      let notes = model ^. #textNotes
          receivedEvents = catMaybes $ getEvent . fst <$> rs
          receivedNotes =
            Set.fromList $
              Prelude.filter
                (\e -> e ^. #kind == TextNote && not (isReply e))
                receivedEvents
          allNotes = notes `Set.union` receivedNotes
          deletionEvents =
            catMaybes $
              Prelude.filter ((== Delete) . kind) receivedEvents
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

          newNotes = Set.toList (updatedNotes `Set.difference` notes)
       in newModel <# do
            load rl $ eventId <$> newNotes
            load pl $ pubKey <$> newNotes
            pure $ SubscribeForReplies newNotes
    SubscribeForReplies es ->
      effectSub model $ subscribeForEventsReplies nn es Home
    ReceivedReactions rs ->
      -- traceM "Got reactions my boy"
      let reactions = model ^. #reactions
       in trace ("  " <> show (length rs)) $
            noEff $
              model
                & #reactions
                .~ Prelude.foldl processReceived reactions rs
    ReceivedProfiles rs ->
      let profiles = (\(xo, pro, when, rel) -> (xo, (pro, when))) <$> rs
       in noEff $
            model
              & #profiles
              %~ unionWith
                ( \p1@(_, d1) p2@(_, d2) ->
                    -- prefer most recent profile
                    if d1 > d2 then p1 else p2
                )
                (fromList profiles)
    GoPage page ->
      noEff $ model & #page .~ page & #history %~ (page :)
    GoBack ->
      let updated = do
            (_, xs) <- uncons $ model ^. #history
            (togo, rest) <- trace ("branko-togo " <> show xs) $ uncons xs
            pure $
              trace ("branko-togo " <> show togo) $
                model & #page .~ togo & #history .~ (togo : rest)
       in noEff $ fromMaybe model updated
    Unfollow xo ->
      let updated = Prelude.filter (/= xo) $ model ^. #contacts
       in (model & #contacts .~ updated)
            <# (updateContacts updated >> pure NoAction)
    WriteModel m ->
      model <# (writeModelToStorage m >> pure NoAction)
    ActualTime t -> noEff $ model & #now .~ t
    DisplayThread e -> do
      effectSub model $ \sink -> do
        forkJSM $ subscribeForWholeThread nn e (ThreadPage e) sink
        liftIO . sink . GoPage $ ThreadPage e
    ThreadEvents es -> do
      -- if there is no root eid in tags then this is a "top-level" note
      -- and so it's eid is the root of the thread
      let getReid e = RootEid $ fromMaybe (e ^. #eventId) $ findRootEid e
          updateModel (e, rel) model =
            let reid = getReid e
                thread = fromMaybe newThread $ model ^. #thread % at reid
                updatedThread = addToThread (e, rel) thread
             in model & #thread % at reid .~ (Just updatedThread)

          updated = Prelude.foldr updateModel model es
      updated <# do
        load rl (eventId . fst <$> es)
        load pl (pubKey . fst <$> es)
        pure NoAction
    UpdateField l v -> noEff $ model & l .~ v
    FindProfile ->
      let xo =
            either (const Nothing) Just $
              decodeNpub $
                model ^. #fpm % #findWho
          updatedModel = model & #fpm % #lookingFor .~ xo
          -- maybe (pure ()) (load pl . singleton) xo
          -- pure NoAction
          runSubscriptions = do
            xo' <- xo
            pure $
              effectSub updatedModel $ \sink -> do
                subscribe
                  nn
                  [DatedFilter (MetadataFilter [xo']) Nothing Nothing]
                  ReceivedProfiles
                  (Just . SubState $ ProfilePage)
                  extractProfile
                  sink
                forkJSM $
                  subscribe -- TODO: need to take Deletes into account
                    nn
                    [DatedFilter (TextNoteFilter [xo']) Nothing Nothing] -- TODO: Nothing Nothing
                    ProfileEvents
                    (Just . SubState $ ProfilePage)
                    (\(res, rel) -> (,rel) <$> getEventOrError res)
                    sink
       in fromMaybe
            (noEff $ updatedModel)
            runSubscriptions
    SubState p st ->
      let updateListWith (sid, ss) list =
            (sid, ss) : filter (\(sid2, _) -> sid2 /= sid) list
          updatedModel =
            model
              & #subscriptions
              % at p
              %~ Just
              . fromMaybe []
              . fmap (updateListWith st)
       in updatedModel <# pure NoAction
    ProfileEvents es ->
      let add (e, r) toThese =
            toThese & at e %~ \er ->
              case er of
                Nothing -> Just $ Set.singleton r
                Just relays -> Just $ Set.insert r relays
          addEvents toThese =
            foldr add toThese es
       in noEff $ model & #fpm % #events %~ addEvents
    DisplayProfilePage xo ->
      let fpm = FindProfileModel (fromMaybe "" $ bechNpub xo) (Just xo) Map.empty
       in effectSub
            (model & #fpm .~ fpm)
            ( \sink -> do
                liftIO . sink $ FindProfile
                liftIO . sink $ GoPage ProfilePage
            )
    _ -> noEff model

-- subscriptions below are parametrized by Page. The reason is
-- so that one can within that page track the state (Running, EOS)
-- of those subscriptions
subscribeForWholeThread :: NostrNetwork -> Event -> Page -> Sub Action
subscribeForWholeThread nn e page sink = do
  subscribeForLinkedEvents nn [(e ^. #eventId)] page sink

subscribeForEventsReplies :: NostrNetwork -> [Event] -> Page -> Sub Action
subscribeForEventsReplies nn es page sink = do
  subscribeForLinkedEvents nn (eventId <$> es) page sink

subscribeForLinkedEvents :: NostrNetwork -> [EventId] -> Page -> Sub Action
subscribeForLinkedEvents nn eids page sink = do
  subscribe
    nn
    [anytime $ LinkedEvents eids]
    ThreadEvents
    (Just $ SubState page)
    process
    sink
  where
    process (resp, rel) =
      maybe (Left "could not extract Event") Right $ do
        evt <- getEvent resp
        pure (evt, rel)

writeModelToStorage :: Model -> JSM ()
writeModelToStorage m = pure ()

updateContacts :: [XOnlyPubKey] -> JSM ()
updateContacts xos = do
  setLocalStorage "my-contacts" $ xos

loadContacts :: JSM [XOnlyPubKey]
loadContacts = fromRight [] <$> getLocalStorage "my-contacts"

secs :: Int -> Int
secs = (* 1000000)

appView :: Model -> View Action
appView m =
  div_ [] $
    [ div_
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
    $ displayProfile <$> loadedProfiles
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
            [class_ "unfollow-button", onClick (Unfollow xo)]
            [text "Unfollow"],
          div_
            [class_ "pic-container"]
            [displayProfilePic xo $ p ^. #picture],
          div_
            [class_ "info-container"]
            [ div_ [class_ "name"] [text $ p ^. #username],
              div_ [class_ "about"] [text . fromMaybe "" $ p ^. #about]
            ]
        ]

notesView :: Model -> View Action
notesView m@Model {..} =
  div_
    [class_ "notes-container"]
    (displayNote m <$> (orderByAgeAsc $ Set.toList textNotes))

footerView :: Model -> View action
footerView Model {..} =
  div_
    [class_ "footer"]
    [ p_
        [style_ $ M.fromList [("font-weight", "bold")]]
        [text err | not . S.null $ err]
    ]

displayProfilePic :: XOnlyPubKey -> Maybe Picture -> View Action
displayProfilePic xo (Just pic) =
  img_
    [ class_ "profile-pic",
      prop "src" $ pic,
      onClick $ DisplayProfilePage xo
    ]
displayProfilePic _ _ = div_ [class_ "profile-pic"] []

displayNoteShort :: Model -> Event -> View Action
displayNoteShort m e =
  div_
    [class_ "text-note"]
    [ p_ [] [text (e ^. #content)],
      div_
        [class_ "text-note-properties"]
        ( maybe [] repliesCount replies
            ++ [reactionsView reactions]
        )
    ]
  where
    eid = e ^. #eventId
    reactions = m ^. #reactions % #processed % at eid
    reid = RootEid $ fromMaybe eid $ findRootEid e
    replies = do
      thread <- m ^. #thread % at reid
      Set.size <$> thread ^. #replies % at eid
    repliesCount c =
      [ div_
          [class_ "replies-count", onClick $ DisplayThread e]
          [text $ "▶ " <> (S.pack . show $ c)]
      ]

displayNote :: Model -> Event -> View Action
displayNote m e =
  div_
    [class_ "note-container"]
    [ div_
        [class_ "profile-pic-container"]
        [displayProfilePic (e ^. #pubKey) $ picUrl m e],
      div_
        [class_ "text-note-container"]
        [ div_ [class_ "profile-info"] [profileName, displayName, noteAge],
          displayNoteShort m e
        ],
      div_ [class_ "text-note-right-panel"] []
    ]
  where
    profile = fromMaybe unknown $ getAuthorProfile m e
    unknown = Profile "" Nothing Nothing Nothing Nothing
    profileName = span_ [id_ "username"] [text $ profile ^. #username]
    displayName = span_ [id_ "display-name"] [text . fromMaybe "" $ profile ^. #displayName]
    noteAge = span_ [id_ "note-age"] [text . S.pack $ displayAge (m ^. #now) e]

rightPanel :: Model -> View Action
rightPanel m =
  div_
    [class_ "right-panel"]
    [div_ [class_ "button-back", onClick (GoBack)] [text "←"], displayPage]
  where
    displayPage = case m ^. #page of
      Home -> notesView m
      Following -> followingView m
      ThreadPage e -> displayThread m e
      ProfilePage -> displayProfilePage m

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

displayProfile :: Model -> XOnlyPubKey -> View Action
displayProfile m xo =
  let notFound =
        div_
          []
          [ text $
              "Profile of "
                <> (S.pack $ show xo)
                <> " not found."
          ]
      profileDisplay = do
        (p, _) <- m ^. #profiles % at xo
        let banner =
              p ^. #banner >>= \b ->
                pure $
                  img_ [class_ "banner-pic", prop "src" b]
        let bannerDef = div_ [class_ "banner-pic-default"] []
        let profilepic =
              p ^. #picture >>= \pic ->
                pure $
                  img_ [class_ "profile-pic", prop "src" pic]
        let profilepicDef = div_ [class_ "profile-pic-default"] []
        let profileName = span_ [id_ "username"] [text $ p ^. #username]
        let displayName =
              span_
                [id_ "display-name"]
                [text . fromMaybe "" $ p ^. #displayName]
        let notes = Map.keys $ m ^. #fpm % #events
        let notesDisplay =
              div_ [class_ "profile-notes"] $ displayNote m <$> notes
        pure $
          div_
            []
            [ div_
                [class_ "banner"]
                [fromMaybe bannerDef banner],
              div_
                [class_ "profile-pic-container"]
                [ fromMaybe profilepicDef profilepic,
                  div_ [] [profileName, displayName]
                ],
              div_
                [class_ "profile-about"]
                [ div_
                    [class_ "about"]
                    [span_ [] [text . fromMaybe "" $ p ^. #about]]
                ],
              notesDisplay
            ]
   in div_
        [class_ "profile-page"]
        [fromMaybe notFound profileDisplay]

displayThread :: Model -> Event -> View Action
displayThread m e =
  let reid = RootEid $ fromMaybe (e ^. #eventId) $ findRootEid e
      -- TODO: This does not work for direct replies to Root.
      -- I suspect they don't have Root set. Only Reply
      parentDisplay = do
        thread <- m ^. #thread % at reid
        parentId <- thread ^. #parents % at (e ^. #eventId)
        parent <- thread ^. #events % at parentId
        pure $ div_ [class_ "parent"] [displayNote m (fst parent)]

      noteDisplay =
        Just $
          div_
            [ class_ $
                if isJust parentDisplay
                  then "note"
                  else "note-no-parent"
            ]
            [displayNote m e]
      repliesDisplay = do
        thread <- m ^. #thread % at reid
        let replies = getRepliesFor thread (e ^. #eventId)
        pure $ (\r -> (div_ [class_ "reply"] [displayNote m r])) <$> replies
   in div_ [class_ "thread-container"] $
        catMaybes [parentDisplay, noteDisplay] ++ fromMaybe [] repliesDisplay

reactionsView :: Maybe (Map Sentiment (Set.Set XOnlyPubKey)) -> View action
reactionsView Nothing = div_ [class_ "reactions-container"] [text ("")]
reactionsView (Just reactions) =
  let howMany = S.pack . show . length . fromMaybe Set.empty
      likes = "♥ " <> howMany (reactions ^. at Like)
      dislikes = "🖓 " <> howMany (reactions ^. at Dislike)
      others = "Others: " <> howMany (reactions ^. at Other)
   in div_
        [class_ "reactions-container"]
        [text (likes <> " " <> dislikes <> " " <> others)]

displayProfilePage :: Model -> View Action
displayProfilePage m =
  let npub = m ^. #fpm % #findWho
      search =
        input_
          [ class_ "input-xo",
            value_ npub,
            type_ "text",
            onInput $ UpdateField (#fpm % #findWho),
            onEnter $ FindProfile
          ]
      ppage =
        (m ^. #fpm % #lookingFor)
          >>= \xo -> pure [displayProfile m xo]
   in div_ [class_ "find-profile"] $
        [div_ [class_ "search-box"] [search]] ++ fromMaybe [] ppage
  where
    onEnter :: Action -> Attribute Action
    onEnter action = onKeyDown $ bool NoAction action . (== KeyCode 13)

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
