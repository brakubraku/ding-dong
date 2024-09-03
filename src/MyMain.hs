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
import Data.Maybe (catMaybes, fromMaybe)
import qualified Data.Set as Set
import Data.Time
import Debug.Trace (trace)
import GHC.Generics
import Miso hiding (now, at, send)
import Miso.String (MisoString)
import qualified Miso.String as S
import MisoSubscribe (subscribe)
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
    -- savedContacts = (\p -> decodeHex p >>= parseXOnlyPubKey) <$> pubKeys
    initialModel contacts actualTime =
      Model
        Set.empty
        (Reactions Map.empty Map.empty)
        "No error yet bitch!"
        []
        -- (catMaybes savedContacts)
        contacts
        Map.empty
        Home
        actualTime
    events = defaultEvents
    view = appView
    mountPoint = Nothing
    logLevel = Off

data Action
  = RelayConnected RelayURI
  | ResponseReceived SubscriptionId [(Response, RelayURI)]
  | TextNotesAndDeletes [(Response, Relay)]
  | HandleWebSocket (WebSocket ())
  | ReceivedProfiles [(Response, Relay)]
  | ReceivedReactions [(ReactionEvent, Relay)]
  | NoAction
  | StartAction
  | GoPage Page
  | Unfollow XOnlyPubKey
  | WriteModel Model
  | ActualTime UTCTime

data Page = Home | Following deriving (Eq, Generic, ToJSON)

data Model = Model
  { textNotes :: Set.Set Event,
    reactions :: Reactions, -- TODO: what about deleted reactions?
    err :: MisoString,
    intialSubs :: [(Event, [RelayURI])], -- events from your contacts
    contacts :: [XOnlyPubKey],
    profiles :: Map.Map XOnlyPubKey (Profile, DateTime),
    page :: Page,
    now :: UTCTime -- don't know a better way to supply time
  }
  deriving (Eq, Generic)

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
                (\e -> e ^. #kind == TextNote && not (isReplyNote e))
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
      let updatedContacts = Prelude.filter (/= xo) $ model ^. #contacts
       in (model & #contacts .~ updatedContacts)
            <# (updateContacts updatedContacts >> pure NoAction)
    WriteModel m ->
      model <# (writeModelToStorage m >> pure NoAction)
    _ -> noEff model

writeModelToStorage :: Model -> JSM ()
writeModelToStorage m = pure ()

-- setLocalStorage "my-model" $ m

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

displayNote :: Model -> Event -> View a
displayNote m e =
  div_
    [class_ "note-container"]
    [ div_
        [class_ "profile-pic-container"]
        [displayProfilePic $ picUrl m e],
      div_
        [class_ "text-note-container"]
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

onEnter :: Action -> Attribute Action
onEnter action = onKeyDown $ bool NoAction action . (== KeyCode 13)

displayResp :: Response -> String
displayResp r =
  case r of
    EventReceived _ evt -> show (evt ^. #content)
    _ -> show r

-- loadContactsFromDisk :: FilePath -> IO (Maybe (Map.Map XOnlyPubKey (ProfileLoader.Types.Profile, DateTime)))
-- loadContactsFromDisk fileName = do
--   loadedProfilesList <-
--     decode @[(XOnlyPubKey, (ProfileLoader.Types.Profile, DateTime))]
--       <$> LazyBytes.readF  let contacts  Map.keys <$> liftIO . fromJust . loadContactsFromDisk $ "contacts.json"

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
      format s | days > 0 = show days <> "d"
               | hours > 0 = show hours <> "h"
               | minutes > 0 = show minutes <> "m"
               | otherwise = ""
        where 
            days = s `div` (3600 * 24)
            hours = s `div` 3600
            minutes = s `div` 60

              --  in unwords $
        --       -- intersperse " " $
        --       zipWith
        --         (\i t -> if i > 0 then show i <> " " <> t else "")
        --         [days, hours, minutes]
        --         ["d", "h", "m"]
   in format ageSeconds

-- xo <- deriveXon
-- pure $ Keys kp
--  >>= \case
--   Right _ -> pure . Just $ newKeys
--   _ -> do
--     liftIO $ logError "Failed to save new keys"
--     pure Nothing

pubKeys :: [String]
pubKeys =
  [ "02aaac8883e92694118f0043e57d8119b513ce0ae31b116c851c0e955195f30e",
    "1bc70a0148b3f316da33fe3c89f23e3e71ac4ff998027ec712b905cd24f6a411",
    "f66c55b57b987829e5a3c5c2df0aebc3279a56ce9d41ecbf312bd9b32f250612",
    "6d0dddfbef0274c0b8ee587c1061323db75d50bd7933a70365e7483fd2a45016",
    "75f457569d7027f819de92e8bb13795c0febe9750dc3fb1b5c42aeb502d0841d",
    "8f72da02fbc80380cbd8b03533d5913b2a5c9fabc9c37cfdafc344ec335f5d21",
    "5e653220e624e74c35387b8c26d3f35ffb955bf94235e1e7bc53b2569dd9bd21",
    "c69205cdb39b8c3b8b953c1b6282a49c379b52c369452e5aee9c24ca0b58b32a",
    "50fbd82a5221fd9a4a7c117a7ff47f4ec794e9a35b553fd6edc4bacb685d8131",
    "3013f8b30d44d43506ae456b001881d311ce1bf69864dc74a0c3e16ec7aee832",
    "1a3ab0a32fe5b7764b06c6aad6414ec3514ee1442fc1b8423d7818e9028bd933",
    "6ad3e2a34818b153c81f48c58f44e5199e7b4fc8dbe37810a000dce3c90b7740",
    "9b37d4a2a1cb24652dab5b0aab6b574276507133527a826ae7d20e1ef92d3d41",
    "da27f5dd5068704a520a7dcd61bf004ce00596feb56849756845bb30bf00234c",
    "fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52",
    "e9d926146f72aa582bd0a84ede374b656d18b3287b2ce274abb12be062441057",
    "8c667c46b4f20670a52cd5cc41b67f13410660fac78604c6006975453ba70f5b",
    "460c25e682fda7832b52d1f22d3d22b3176d972f60dcdc3212ed8c92ef85065c",
    "d830ee7b7c30a364b1244b779afbb4f156733ffb8c87235086e26b0b4e61cd62",
    "27154fb873badf69c3ea83a0da6e65d6a150d2bf8f7320fc3314248d74645c64",
    "000000dd7a2e54c77a521237a516eefb1d41df39047a9c64882d05bc84c9d666",
    "6b9da920c4b6ecbf2c12018a7a2d143b4dfdf9878c3beac69e39bb597841cc6e",
    "f27c6a9d6e2e0e28115104508d097a04750b357a02c1a7e0ce5bb2fc54210470",
    "14c0aae7882730fb0885723a82ec3010ed8ca46eb65f503d24e3dc3ddc45b472",
    "59aa572a2b7d8569ce019a0a0dfa43251fdc2aa8947b0d69b5a5010789d8dd7a",
    "eb2d6b7b9825a1b35b4cd22de54a90abade29d89542a906fd223d4e8cee3b484",
    "be6fd25772d10b35df54c10e573fd3e383d2e8eaa4a0bddf0d5def578cb70897",
    "76dd32f31619b8e35e9f32e015224b633a0df8be8d5613c25b8838a370407698",
    "345f22270195358dc81564a3790c141df689e48ed8c946c6ce910f6f9fd18699",
    "e56509901ced5e39282ae1748e1cf1477beed17f08495d2ad9bc3f7e60d7fb9b",
    "e64c0224682106e49f0c3b67f59d3df9d3efa282c272173acd8ec291f6d5839e",
    "4e050249d143dac893b8d072a328ac3eb9e14218e717a0e6965bb8a28d23d79f",
    "82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbfbe6a2",
    "975b04dcfb3a72dc5bc64f52cf948b73e71e0cee328c956dc0c340796a8ef0a2",
    "8eea02e8912085962a930b28beed2683a988614de9a339750ae0b3061e2c6db1",
    "a3c488ec1906c556aef094f0c4fd8eeb79dd6909badd500a303234f26e33f6bd",
    "c1831fbe2653f76164421d57db6cee38b8cef8ce6771bc65c12f8543de4b39bf",
    "ddf3ee335bd27b9845547436e2202adadda4349f01827476d512091f5406d5cd",
    "a80fc4a78634ee26aabcac951b4cfd7b56ae18babd33c5afdcf6bed6dc80ebd1",
    "b9148b801147ea86609c4e555fe853b0ba8adbd491c6db75373a9260ae8609d2",
    "f5d70542664e65719b55d8d6250b7d51cbbea7711412dbb524108682cbd7f0d4",
    "dd2dbe6b8c09a4abb9eb18fa2ce174c3ebf311d2ce11251487a32bee477844d7",
    "8c07e2316b9d8b5db37b554b3e2d58cc1dc5ff98d21a826dc4a6f895134127d8",
    "ee772a9ec50f3c664ce3583a23d7f9e036d4e353f11a94b2d11113165d3dced8",
    "0e6e0dab0d20d1033ef6f4f902024be3ef8948f1203c47e300b325e21b836ddc",
    "382d893abf7bb83b09fa1e37cc6e71b27a4e46a3fea57463bffc8c812ee697e7",
    "97ca76334c49ef77e3c7819a4b3ba42c9ce1bd4a7cbdfd35a6cefd532f1a4dec"
  ]
