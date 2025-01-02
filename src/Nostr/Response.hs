{-# LANGUAGE OverloadedStrings #-}

module Nostr.Response where

import Data.Aeson
import Data.Maybe
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Nostr.Event
import Nostr.Relay
import Nostr.Request (SubscriptionId)
import Nostr.HashableEvent

data HashableResponse = HashableEventReceived SubscriptionId HashableEvent

instance FromJSON HashableResponse where
   parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0
    case type' of
      String "EVENT" -> do
        se <- parseJSON $ arr V.! 2
        subid <- parseJSON $ arr V.! 1
        pure $ HashableEventReceived subid se
      _ -> fail "Not an event"

data Response
  = EventReceived SubscriptionId Event
  | Notice Text
  | EOSE SubscriptionId
  | OK EventId Bool (Maybe Text)
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0
    case type' of
      String "EVENT" -> do
        event <- parseJSON $ arr V.! 2
        subid <- parseJSON $ arr V.! 1
        pure $ EventReceived subid event
      String "NOTICE" -> do 
        subid <- parseJSON $ arr V.! 1
        pure $ Notice subid
      String "EOSE" -> do 
        subid <- parseJSON $ arr V.! 1
        pure $ EOSE subid
      String "OK" -> do 
        eid <- parseJSON $ arr V.! 1
        isSuccess <- parseJSON $ arr V.! 2
        failureReason <- parseJSON $ arr V.! 3
        pure $ OK eid isSuccess failureReason
      unknown ->
        fail $ "Uknown response" <> show unknown

instance ToJSON Response where 
   toJSON r = 
    case r of 
      EventReceived sid e -> 
        Array $
          V.fromList
            [ String "EVENT",
              toJSON sid,
              toJSON e
            ]
      EOSE sid ->
        Array $ 
          V.fromList 
            [String "EOSE", toJSON sid]
      _ -> Array $ V.fromList []

getEvent :: Response -> Maybe Event
getEvent (EventReceived _ e) = Just e
getEvent _ = Nothing

getEventRelay ::
  (Response, Relay) -> Maybe (Event, Relay)
getEventRelay (res, rel) = do
  event <- getEvent res
  pure (event, rel)

getEventRelayEither ::
  (Response, Relay) -> Either Text (Event, Relay)
getEventRelayEither rr =
  fromMaybe
    (Left $ "Failed to extract event from response:" <> (T.pack . show $ fst rr))
    $ Right <$> getEventRelay rr

getEventOrError :: Response -> Either Text Event
getEventOrError (EventReceived _ e) = Right e
getEventOrError r = Left $ "Received response:" <> (T.pack . show $ r)
