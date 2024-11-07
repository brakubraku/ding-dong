{-# LANGUAGE OverloadedStrings #-}

module Contacts where

import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Maybe
import Data.Time
import MyCrypto
import Nostr.Event
import Nostr.Keys
import Nostr.Log
import Nostr.Network
import Nostr.Profile
import Nostr.RelayPool
import Nostr.Request
import Data.Text

someContacts :: [XOnlyPubKey]
someContacts = catMaybes $ (\p -> decodeHex p >>= parseXOnlyPubKey) <$> someKeys
-- someContacts = extractProfileIds . catMaybes $ decodeBech <$> pubKeys

-- >>> Prelude.length someContacts
-- 37

someKeys :: [Text]
someKeys =
  [ "6d5f85c4be85fabb803716cc002f46c88ad5d44dae8098f057482640a3f7e400",
    "dc4cd086cd7ce5b1832adf4fdd1211289880d2c7e295bcb0e684c01acee77c06",
    "9ca0bd7450742d6a20319c0e3d4c679c9e046a9dc70e8ef55c2905e24052340b",
    "668ceee55475f595ec0e8ef44c64bf9da8d9dd6008ac54dcd24b2501930b960e",
    "e88a691e98d9987c964521dff60025f60700378a4879180dcbbb4a5027850411",
    "83d999a148625c3d2bb819af3064c0f6a12d7da88f68b2c69221f3a746171d19",
    "4c7f826edf647462f744b3f16d485f53c797eabdb21cc8a7bb0713283b88e629",
    "ffd5fae2e636341f2a8ced7ad3602b3ca021da75313bea3eeb068e378bf3d52f",
    "0aa39e5aef99a000a7bdb0b499158c92bc4aa20fb65931a52d055b5eb6dff738",
    "a3eb29554bd27fca7f53f66272e4bb59d066f2f31708cf341540cb4729fbd841",
    "a9b9525992a486aa16b3c1d3f9d3604bca08f3c15b712d70711b9aecd8c3dc44",
    "baf27a4cc4da49913e7fdecc951fd3b971c9279959af62b02b761a043c33384c",
    "7e17a8b715b6e9bc360f7d82b4ed773a378a2816fc29f456be3cce97ba12794c",
    "fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52",
    "460c25e682fda7832b52d1f22d3d22b3176d972f60dcdc3212ed8c92ef85065c",
    "0697437d5de87dc49924ede4d760dbdf032481206b710023d4f3c5d2e67f5665",
    "060db90965790e88d9c4c5ce339aae4be1ac5f7512bc95a11f30303ae7dbba69",
    "433e80c14ff7b8e16e179ccec35f55833df7dd5a5a063d23117b4b01b6f97170",
    "0a24b1e0d4ed9589a19714cbf0db25224529d1c7dca85866747e466548842275",
    "44a1f09c5aeaa977f3d94061ac7de22a678e53f62e780c3ddaecfb2e3098597a",
    "40dba08627a2f2c69c3031666149b567168f049894aa5c42203a3920a3de8483",
    "645681b9d067b1a362c4bee8ddff987d2466d49905c26cb8fec5e6fb73af5c84",
    "9f006fa3597d3e30cf6824c555d20769db8c862077ed020756dde9a050714585",
    "3aa5bad54fd7bc6b4c434cd24116c361a336969b9430cf70d82cd5712f708f85",
    "874db6d2db7b39035fe7aac19e83a48257915e37d4f2a55cb4ca66be2d77aa88",
    "654576c7142257849ee390b0cdc6d73db9fbf1109c37be6c2da52f9e8405b189",
    "8ee4290c98b20b2999138b322f466ea20c75608f568ab53cf11492bf9b2e849c",
    "82341f882b6eabcd2ba7f1ef90aad961cf074af15b9ef44a09f9d2a8fbfbe6a2",
    "e6a4fe53bdb13e67802a77ce878f6c4deff0d58df9d3142a333b92041aa9eba8",
    "8a699686811889186df398c7253e8c4417ce73fe814edeae7ecd81dbde9536ac",
    "ebdee92945ef05283be0ac3de25787c81a6a58a10f568f9c6b61d9dd513adbad",
    "604e96e099936a104883958b040b47672e0f048c98ac793f37ffe4c720279eb2",
    "04c915daefee38317fa734444acee390a8269fe5810b2241e5e6dd343dfbecc9",
    "c582af78dff442700ec59e21786532a7074c00be8b7b1eac989bbf62698069cc",
    "3b8c97ae9286f01253c4f88d42d16e858c7c92513abf2f38251aff713514bce6",
    "e1ca15f1149ef0cec45d565222c2111b85327ede53357a5c4e7c3e17105aa0fa",
    "c6f7077f1699d50cf92a9652bfebffac05fc6842b9ee391089d959b8ad5d48fd"
  ]

saveContacts :: [(XOnlyPubKey, Maybe Username)] -> NostrNetworkT ()
saveContacts contacts = do
  (Keys sk xo _) <- asks keys
  now <- liftIO getCurrentTime
  let unsigned = setContacts contacts xo now
  case signEvent unsigned sk xo of
    Just signed -> send . SendEvent $ signed
    Nothing -> liftIO . logError $ "Failed signing event!"
