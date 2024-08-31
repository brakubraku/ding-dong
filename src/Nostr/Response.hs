{-# LANGUAGE OverloadedStrings   #-}

module Nostr.Response where

import           Data.Aeson
import           Control.Monad          (mzero)
import           Data.Text              (Text)
import qualified Data.Vector            as V

import Nostr.Event
-- import Nostr.Relay
import Nostr.Request ( SubscriptionId )
import qualified Data.Text as T

data Response
  = EventReceived SubscriptionId Event
  | Notice Text
  | EOSE SubscriptionId
  deriving (Eq, Show)

instance FromJSON Response where
  parseJSON = withArray "ServerResponse" $ \arr -> do
    type' <- parseJSON $ arr V.! 0
    param <- parseJSON $ arr V.! 1
    case type' of
      String "EVENT"  -> do
        event <- parseJSON $ arr V.! 2
        return $ EventReceived param event
      String "NOTICE" -> return $ Notice param
      String "EOSE" -> return $ EOSE param
      _ ->
        mzero

getEvent :: Response -> Maybe Event 
getEvent (EventReceived _ e) = Just e 
getEvent  _ = Nothing

getEventOrError :: Response -> Either Text Event 
getEventOrError (EventReceived _ e) = Right e 
getEventOrError  r = Left $ "Received response:" <> (T.pack . show $ r)

