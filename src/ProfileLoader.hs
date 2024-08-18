{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module ProfileLoader where

import AppEvent
import Control.Concurrent
import Control.Concurrent.STM
import Control.Lens
import Control.Monad.Cont
import Control.Monad.Trans.Accum (accum)
import Crypto.Schnorr
import Data.Aeson
import Data.Bifunctor
import Data.ByteString hiding (pack, unpack)
import qualified Data.ByteString.Lazy as LazyBytes
import Data.DateTime
import Data.Default
import Data.IntMap (updateWithKey)
import Data.List (nub, (\\))
import qualified Data.Map as Map
import Data.Maybe (catMaybes, isNothing, mapMaybe)
import Data.Text
import Data.Traversable
import Debug.Trace
import Helpers (collectJustM)
import Log
import Monomer.Graphics.Lens (HasFontName (fontName), HasImgData (imgData))
import Nostr.Event
import qualified Nostr.Filter as Filter
import Nostr.Network
import qualified Nostr.Profile as Nostr
import Nostr.Relay
import Nostr.RelayPool (subscribeForFilter, unsubscribe)
import Nostr.Response
import qualified Nostr.Utils as Utils
import ProfileLoader.LoadPicUrl
import ProfileLoader.Types (picFolder)
import ProfileLoader.Types hiding (picFolder)
import System.Timeout
import System.Log
import System.Log.Logger
import System.Log.Handler.Simple
import System.Log.Handler ( LogHandler(setFormatter) )
import System.Log.Formatter

type LoadedProfiles = Map.Map XOnlyPubKey (Profile, DateTime)

data Profiles = Profiles
  { _loading :: [XOnlyPubKey],
    _loaded :: LoadedProfiles,
    _loadingPics :: [(XOnlyPubKey, Profile)],
    _changed :: Bool -- mark as changed
  }

makeLenses 'Profiles

newEmptyProfiles :: Profiles
newEmptyProfiles = Profiles [] Map.empty [] False

logMsg :: Priority -> String -> IO () 
logMsg prio m = do 
  logger <- getLogger "ProfileLoader"
  logL logger prio m

-- TODO: start loading pictures only after the widget which contains them
--       is inside viewport - not sure if Scroll widget sends any event to inform
--       it's children that they have become visible/invisible
-- Maybe I can implement a widget container, which checks whether it's viewport is visible and display/hide it's child

loadProfiles :: [XOnlyPubKey] -> MVar Profiles -> IO ()
loadProfiles xos profilesMVar = do
  -- logMsg DEBUG  $ "Requesting to load profiles " <> show xos
  profiles <- takeMVar profilesMVar
  let toLoad = (xos \\ Map.keys (profiles ^. loaded)) \\ profiles ^. loading
  -- logMsg DEBUG  $ "And proceeding to load profiles " <> show toLoad
  logMsg DEBUG $ "actually loading=" <> show toLoad
  putMVar profilesMVar $ profiles & loading %~ (++) toLoad 

writeProfilesToDisk :: LoadedProfiles -> String -> IO ()
writeProfilesToDisk profiles fileName = do
  LazyBytes.writeFile fileName . encode . Map.toList $ profiles

loadProfilesFromDisk :: String -> MVar Profiles -> IO (Either String Profiles)
loadProfilesFromDisk fileName profilesMVar = do
  loadedProfilesList <-
    decode @[(XOnlyPubKey, (Profile, DateTime))]
      <$> LazyBytes.readFile fileName
  let mLoadedProfiles = Map.fromList <$> loadedProfilesList
  case mLoadedProfiles of
    Nothing ->
      pure . Left $ "Failed loading profiles from disk"
    Just loadedFromFile ->
      Right <$> updateLoadedProfiles profilesMVar loadedFromFile

updateLoadedProfiles :: MVar Profiles -> Map.Map XOnlyPubKey (Profile, DateTime) -> IO Profiles
updateLoadedProfiles mvar withThese =
  do
    modifyMVar mvar $ \profiles -> do
      let updatedProfiles =
            profiles
              & loaded
                .~ Map.unionWithKey preferNewer (profiles ^. loaded) withThese
              & changed .~ True
      pure (updatedProfiles, updatedProfiles)

preferNewer :: k -> (Profile, DateTime) -> (Profile, DateTime) -> (Profile, DateTime)
preferNewer _ p1@(_, when1) p2@(_, when2) = if when1 > when2 then p1 else p2

profileLoadTimeout :: Int
profileLoadTimeout = 5 * 10 ^ 6

loadPics :: MVar Profiles -> [(XOnlyPubKey, Profile)] -> IO ()
loadPics mvar ps = do
  let dontDownload p =
        hasDownloadedPic p || isNothing (p ^. profilePic . url)

      toDownload = Prelude.filter (not . dontDownload . snd) ps

  logMsg DEBUG $ "Loading these pics:" <> show toDownload
  modifyMVar_ mvar $ \profiles -> pure $ profiles & loadingPics %~ (nub . (++) toDownload)

startLoadingPics :: MVar Profiles -> IO ()
startLoadingPics psMV = do
  sessMV <- createSession
  -- todo: replace void with something like "wait until all forks terminate"
  void . forkIO . forever $ do
    toLoad <- modifyMVar psMV $ \profiles -> do
      pure (profiles & loadingPics .~ [], profiles ^. loadingPics)
    -- todo: run this in more threads and wait for them to finish
    -- logMsg DEBUG $ "loading profile pics for=" <> show toLoad
    mapM_ (loadPic sessMV) toLoad
    threadDelay $ 10 ^ 5 * 2
  where
    loadPic sessMV (xo, p) =
      trace ("Loading pic for: " <> show p) $
        let murl = p ^. profilePic . url
         in case murl of
              Nothing -> pure ()
              Just url -> do
                res <- loadRemote sessMV url
                case res of
                  Left error ->
                    logMsg ERROR $ "(" <> show error <> ") loading pic for profile" <> show p
                  Right img -> do
                    now <- getCurrentTime
                    fn <- saveProfilePic xo img
                    let updatedP =
                          p & profilePic . filename .~ fn
                            & profilePic . fetchedAt ?~ now
                    logMsg DEBUG $ "downloaded pic for profile " <> show updatedP
                    updateLoaded psMV xo updatedP

saveProfilePic :: XOnlyPubKey -> ByteString -> IO Text
saveProfilePic xo img = do
  let fn = picFolder <> "/" <> (pack . show) xo
  Data.ByteString.writeFile (unpack fn) img
  pure fn

updateLoaded :: MVar Profiles -> XOnlyPubKey -> Profile -> IO ()
updateLoaded mvar xo p = do
  modifyMVar_ mvar $ \profiles ->
    pure $
      profiles
        & loaded . at xo %~ fmap (first (const p))
        & changed .~ True

runPeriodically :: MonadIO m => Int -> m () -> m ()
runPeriodically period act =
  void . forever $ do
    act
    liftIO $ threadDelay period

refresh :: MVar Profiles -> (AppEvent -> IO ()) -> IO ()
refresh mvar sendMsg = do
  logMsg DEBUG "Refreshing frontend"
  (updatedLoaded, hasChanged) <- modifyMVar mvar $ \profiles ->
    pure (profiles & changed .~ False, (profiles ^. loaded, profiles ^. changed))
  when hasChanged $ do
    logMsg DEBUG $ "Profiles changed. refreshing frontend with" <> show updatedLoaded
    sendMsg $ NewProfiles updatedLoaded
    writeProfilesToDisk updatedLoaded "profiles.json"

startProfileLoader :: MVar Profiles -> (AppEvent -> IO ()) -> NostrNetworkT ()
startProfileLoader profilesMVar sendMsg = do
  -- lift . forkIO $ startLoadingPics profilesMVar
  lift . forkIO . runPeriodically (10 ^ 6) $ refresh profilesMVar sendMsg -- inform widgets every second
  void . lift $ do
    loadedFromDisk <- loadProfilesFromDisk "profiles.json" profilesMVar

    let sendMonomerEvents = \profiles -> do
          -- TODO: decouple this out from this module
          sendMsg . NewProfiles $ profiles ^. loaded
    either (logMsg ERROR) sendMonomerEvents loadedFromDisk

  runPeriodically (10 ^ 5 * 5) $ do
    profilesToLoad <- lift . modifyMVar profilesMVar $ \profiles -> do
      let newLoaded = Prelude.foldr insertEmpty (profiles ^. loaded) (profiles ^. loading)
      let result =
            profiles & loading .~ []
              & loaded .~ newLoaded
      pure (result, profiles ^. loading)
    lift $ logMsg DEBUG $ "Subscribing for profiles " <> show profilesToLoad
    subscribeForProfiles profilesToLoad
  where
    insertEmpty ::
      XOnlyPubKey ->
      LoadedProfiles ->
      LoadedProfiles
    insertEmpty xo profilesMap =
      let emptyProfile = fromNostr xo def
       in -- bro I don't know how to put a less verbose date here
          Map.insert xo (emptyProfile, read "2000-01-01 10:00:00 UTC") profilesMap

    subscribeForProfiles :: [XOnlyPubKey] -> NostrNetworkT ()
    subscribeForProfiles xos
      | Prelude.null xos = pure ()
      | otherwise = do
        now <- lift getCurrentTime
        let filter = [Utils.until now (Filter.MetadataFilter xos)]
        (respCh, subId) <- subscribeForFilter filter
        void . lift . timeout profileLoadTimeout $ do
          runPeriodically (10 ^ 5 * 5) $ do
            msg <- atomically $ readTChan respCh
            msgs <- collectJustM . atomically $ tryReadTChan respCh
            let receivedProfiles = mapMaybe extractProfile (msg : msgs)
            logMsg DEBUG $ "received profiles=" <> show receivedProfiles
            void . updateLoadedProfiles profilesMVar $ Map.fromList receivedProfiles
            loadPics profilesMVar $ (\(x, (p, _)) -> (x, p)) <$> receivedProfiles
        unsubscribe subId

extractProfile :: (Response, Relay) -> Maybe (XOnlyPubKey, (Profile, DateTime))
extractProfile (EventReceived _ event, _) = parseProfiles event
  where
    parseProfiles e =
      let xo = Nostr.Event.pubKey e
       in case readProfile e of
            Just p -> Just (xo, (fromNostr xo p, created_at e))
            Nothing -> Nothing
extractProfile _ = Nothing

getPicUrl :: Map.Map XOnlyPubKey (Profile, DateTime) -> XOnlyPubKey -> Maybe Text
getPicUrl map xo =
  map ^? at xo . _Just . _1 . profilePic . filename