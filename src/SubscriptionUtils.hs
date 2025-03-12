module SubscriptionUtils where

import Control.Concurrent
import Data.Text
import MisoSubscribe
import ModelAction
import Nostr.Event
import Nostr.Filter
import Nostr.Relay
import Nostr.Response
import ProfilesLoader (extractProfile)
import Utils
import ProfilesLoader.Types

periodic ::
  [DatedFilter] ->
  ([e] -> action) ->
  (Text -> action) ->
  ((Response, Relay) -> Either Text e) ->
  SubscriptionParams action
periodic filter actOnResults actOnError extractResults =
  SubscriptionParams
    { subType = PeriodicUntilEOS,
      subFilter = filter,
      extractResults = extractResults,
      actOnResults = actOnResults,
      actOnSubState = Nothing,
      cancelButton = Nothing,
      timeoutPerRelay = Just . Seconds $ 15,
      reportError = actOnError
    }

periodicUntilEOSOnPage ::
  Page ->
  [DatedFilter] ->
  ([(Event, Relay)] -> Action) ->
  SubscriptionParams Action
periodicUntilEOSOnPage page filter actOnResults =
  SubscriptionParams
    { subType = PeriodicUntilEOS,
      subFilter = filter,
      extractResults = getEventRelayEither,
      actOnResults = actOnResults,
      actOnSubState = Just $ SubState page,
      cancelButton = Nothing,
      timeoutPerRelay = Just . Seconds $ 15,
      reportError = reportErrorAction
    }

periodicLoadProfileOnPage ::
  Page ->
  [DatedFilter] ->
  ([ProfOrRelays] -> Action) ->
  SubscriptionParams Action
periodicLoadProfileOnPage page filter actOnResults =
  SubscriptionParams
    { subType = PeriodicUntilEOS,
      subFilter = filter,
      extractResults = extractProfile,
      actOnResults = actOnResults,
      actOnSubState = Just $ SubState page,
      cancelButton = Nothing,
      timeoutPerRelay = Just . Seconds $ 15,
      reportError = reportErrorAction
    }

periodicForever ::
  [DatedFilter] ->
  ([(Event, Relay)] -> Action) ->
  Maybe (MVar ()) ->
  SubscriptionParams Action
periodicForever filter actOnResults cb =
  SubscriptionParams
    { subType = PeriodicForever,
      subFilter = filter,
      extractResults = getEventRelayEither,
      actOnResults = actOnResults,
      actOnSubState = Nothing,
      cancelButton = cb,
      timeoutPerRelay = Nothing,
      reportError = reportErrorAction
    }

allAtEOS ::
  [DatedFilter] ->
  ([(Event, Relay)] -> Action) ->
  SubscriptionParams Action
allAtEOS filter actOnResults =
  SubscriptionParams
    { subType = AllAtEOS,
      subFilter = filter,
      extractResults = getEventRelayEither,
      actOnResults = actOnResults,
      actOnSubState = Nothing,
      cancelButton = Nothing,
      timeoutPerRelay = Nothing,
      reportError = reportErrorAction
    }

allAtEOSOnPage ::
  Page ->
  [DatedFilter] ->
  ([(Event, Relay)] -> Action) ->
  SubscriptionParams Action
allAtEOSOnPage page filter actOnResults =
  SubscriptionParams
    { subType = AllAtEOS,
      subFilter = filter,
      extractResults = getEventRelayEither,
      actOnResults = actOnResults,
      actOnSubState = Just $ SubState page,
      cancelButton = Nothing,
      timeoutPerRelay = Nothing,
      reportError = reportErrorAction
    }

reportErrorAction :: Text -> Action
reportErrorAction = Report ErrorReport
