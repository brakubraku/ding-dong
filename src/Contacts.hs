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
import BechUtils (decodeBech, extractProfileIds)
import Data.Text

someContacts :: [XOnlyPubKey]
someContacts = catMaybes $ (\p -> decodeHex p >>= parseXOnlyPubKey) <$> someKeys
-- someContacts = extractProfileIds . catMaybes $ decodeBech <$> pubKeys

-- >>> Prelude.length someContacts
-- 19

someKeys :: [Text]
someKeys =
  [ "9ca0bd7450742d6a20319c0e3d4c679c9e046a9dc70e8ef55c2905e24052340b",
    "8fb140b4e8ddef97ce4b821d247278a1a4353362623f64021484b372f948000c",
    "668ceee55475f595ec0e8ef44c64bf9da8d9dd6008ac54dcd24b2501930b960e",
    "e88a691e98d9987c964521dff60025f60700378a4879180dcbbb4a5027850411",
    "1bc70a0148b3f316da33fe3c89f23e3e71ac4ff998027ec712b905cd24f6a411",
    "a3eb29554bd27fca7f53f66272e4bb59d066f2f31708cf341540cb4729fbd841",
    "a9b9525992a486aa16b3c1d3f9d3604bca08f3c15b712d70711b9aecd8c3dc44",
    "fa984bd7dbb282f07e16e7ae87b26a2a7b9b90b7246a44771f0cf5ae58018f52",
    "460c25e682fda7832b52d1f22d3d22b3176d972f60dcdc3212ed8c92ef85065c",
    "deab79dafa1c2be4b4a6d3aca1357b6caa0b744bf46ad529a5ae464288579e68",
    "433e80c14ff7b8e16e179ccec35f55833df7dd5a5a063d23117b4b01b6f97170",
    "0a24b1e0d4ed9589a19714cbf0db25224529d1c7dca85866747e466548842275",
    "9f006fa3597d3e30cf6824c555d20769db8c862077ed020756dde9a050714585",
    "8a699686811889186df398c7253e8c4417ce73fe814edeae7ecd81dbde9536ac",
    "604e96e099936a104883958b040b47672e0f048c98ac793f37ffe4c720279eb2",
    "b8a9df8218084e490d888342a9d488b7cf0fb20b1a19b963becd68ed6ab5cbbd",
    "04c915daefee38317fa734444acee390a8269fe5810b2241e5e6dd343dfbecc9",
    "34651a9392d34bf9d064e39f3c10beef74b5e28e710c07324317b687c4abd7e8",
    "c6f7077f1699d50cf92a9652bfebffac05fc6842b9ee391089d959b8ad5d48fd"
  ]

pubKeys :: [Text]
pubKeys = [
  -- "npub1xtscya34g58tk0z605fvr788k263gsu6cy9x0mhnm87echrgufzsevkk5s",
  --   "npub1sg6plzptd64u62a878hep2kev88swjh3tw00gjsfl8f237lmu63q0uf63m",
  --   "npub1j8y6tcdfw3q3f3h794s6un0gyc5742s0k5h5s2yqj0r70cpklqeqjavrvg",
  --   "npub1csamkk8zu67zl9z4wkp90a462v53q775aqn5q6xzjdkxnkvcpd7srtz4x9",
  --   "npub1qqqqqqyz0la2jjl752yv8h7wgs3v098mh9nztd4nr6gynaef6uqqt0n47m",
  --   "npub1qny3tkh0acurzla8x3zy4nhrjz5zd8l9sy9jys09umwng00manysew95gx",
  --   "npub1dergggklka99wwrs92yz8wdjs952h2ux2ha2ed598ngwu9w7a6fsh9xzpc",
  --   "npub1y24gz5gwucl79vtv4ctwpysl0r5m4xyzu2rgulnr44ks3t5mt92q4nz2ad",
  --   "npub1z4m7gkva6yxgvdyclc7zp0vz4ta0s2d9jh8g83w03tp5vdf3kzdsxana6p",
  --   "npub180cvv07tjdrrgpa0j7j7tmnyl2yr6yr7l8j4s3evf6u64th6gkwsyjh6w6",
  --   "npub18ams6ewn5aj2n3wt2qawzglx9mr4nzksxhvrdc4gzrecw7n5tvjqctp424",
  --   "npub107jk7htfv243u0x5ynn43scq9wrxtaasmrwwa8lfu2ydwag6cx2quqncxg",
  --   "npub1sn0wdenkukak0d9dfczzeacvhkrgz92ak56egt7vdgzn8pv2wfqqhrjdv9",
  --   "npub1tvqc82mv8cezhax5r34n4muc2c4pgjz8kaye2smj032nngg52clq0rkrq4",
  --   "npub1cj8znuztfqkvq89pl8hceph0svvvqk0qay6nydgk9uyq7fhpfsgsqwrz4u",
  --   "npub1hu3hdctm5nkzd8gslnyedfr5ddz3z547jqcl5j88g4fame2jd08qh6h8nh",
  --   "npub16vrkgd28wq6n0h77lqgu8h4fdu0eapxgyj0zqq6ngfvjf2vs3nuq5mp2va",
  --   "npub1a2cww4kn9wqte4ry70vyfwqyqvpswksna27rtxd8vty6c74era8sdcw83a",
  --   "npub1gcxzte5zlkncx26j68ez60fzkvtkm9e0vrwdcvsjakxf9mu9qewqlfnj5z",
  --   "npub1cjw49ftnxene9wdxujz3tp7zspp0kf862cjud4nm3j2usag6eg2smwj2rh",
  --   "npub1sqaxzwvh5fhgw9q3d7v658ucapvfeds3dcd2587fcwyesn7dnwuqt2r45v",
  --   "npub1jt0x3vsnqtazzda3ewa8ykdch2t8k566qhrd9vyy0k0ntleu744q8h6q3n",
  --   "npub17u5dneh8qjp43ecfxr6u5e9sjamsmxyuekrg2nlxrrk6nj9rsyrqywt4tp",
  --   "npub1cn4t4cd78nm900qc2hhqte5aa8c9njm6qkfzw95tszufwcwtcnsq7g3vle",
  --   "npub1hqaz3dlyuhfqhktqchawke39l92jj9nt30dsgh2zvd9z7dv3j3gqpkt56s",
  --   "npub1clk6vc9xhjp8q5cws262wuf2eh4zuvwupft03hy4ttqqnm7e0jrq3upup9",
  --   "npub1g53mukxnjkcmr94fhryzkqutdz2ukq4ks0gvy5af25rgmwsl4ngq43drvk",
  --   "npub12rv5lskctqxxs2c8rf2zlzc7xx3qpvzs3w4etgemauy9thegr43sf485vg",
  --   "npub1r0rs5q2gk0e3dk3nlc7gnu378ec6cnlenqp8a3cjhyzu6f8k5sgs4sq9ac",
    "npub1v6xwae25wh6etmqw3m6yce9lnk5dnhtqpzk9fhxjfvjsryctjc8q2kxk5t",
    "npub13pnmakf738yn6rv2ex9jgs7924renmderyp5d9rtztsr7ymxg3gqej06vw",
    "npub1l2vyh47mk2p0qlsku7hg0vn29faehy9hy34ygaclpn66ukqp3afqutajft",
    "npub14xu4ykvj5jr2594nc8fln5mqf09q3u7ptdcj6ur3rwdwekxrm3zq86428l",
    "npub1504jj42t6flu5l6n7e389e9mt8gxduhnzuyv7dq4gr95w20mmpqscx0cg0",
    "npub137c5pd8gmhhe0njtsgwjgunc5xjr2vmzvglkgqs5sjeh972gqqxqjak37w",
    "npub13f5edp5przy3sm0nnrrj205vgstuuul7s98datn7ekqahh54x6kqufgvmt",
    "npub1hz5alqscpp8yjrvgsdp2n4ygkl8slvstrgvmjca7e45w6644ew7sewtysa",
    "npub1az9xj85cmxv8e9j9y80lvqp97crsqdu2fpu3srwthd99qfu9qsgstam8y8",
    "npub1gvlgps2077uwzmshnn8vxh64sv7l0h26tgrr6gc30d9srdhew9cqxcnhgv",
    -- "npub1tlgqfynfdyup4s4m8ge8yqpkm8ukxtfflv3dcxl4mra3e83x27vqrlj5tp",
    "npub1nuqxlg6e05lrpnmgynz4t5s8d8dcep3qwlksyp6kmh56q5r3gkzssrnl5j",
    "npub1q3sle0kvfsehgsuexttt3ugjd8xdklxfwwkh559wxckmzddywnws6cd26p",
    "npub1m64hnkh6rs47fd9x6wk2zdtmdj4qkazt734d22d94ery9zzhne5qw9uaks",
    "npub1pgjtrcx5ak2cngvhzn9lpke9yfzjn5w8mj59sen50erx2jyyyf6s4hqm7z"
  ]
  
saveContacts :: [(XOnlyPubKey, Maybe Username)] -> NostrNetworkT ()
saveContacts contacts = do
  (Keys sk xo _) <- asks keys
  now <- liftIO getCurrentTime
  let unsigned = setContacts contacts xo now
  case signEvent unsigned sk xo of
    Just signed -> send . SendEvent $ signed
    Nothing -> liftIO . logError $ "Failed signing event!"
