
-- |
-- Module      : Puppet
-- Description : Utilities for generating Puppeteer client code with injected localStorage for browser automation tests.
--               Includes functions to create, write, and test dynamic JS code for Puppeteer, with escaping for JS string literals.
--
-- Main functions:
--   - createPuppeteer: Injects localStorage JS into a Puppeteer client template.
--   - writePuppeteerClient: Writes generated client code to a file.
--   - generateLocalStorageJs: Produces JS code from a Haskell Map for localStorage injection.
--   - escapeJs: Escapes single quotes and backslashes for JS string safety.
--   - test* functions: Demonstrate and test escaping and code generation.
--
module Puppet where

import System.IO (writeFile)
import qualified Data.Map as Map

-- | Template for Puppeteer client.js code, with a placeholder for localStorage injection
clientJsTemplate :: String
clientJsTemplate = unlines
  [ "const puppeteer = require('puppeteer');"
  , "(async () => {"
  , "  const browser = await puppeteer.launch({ headless: true });"
  , "  const page = await browser.newPage();"
  , "  // Inject localStorage block here"
  , "  __LOCAL_STORAGE__"
  , "  await page.goto('http://localhost:1234', {});"
  , "  // ... rest of your client.js logic ..."
  , "})();"
  ]

-- | Create Puppeteer client.js code with injected localStorage JS block
createPuppeteer :: String -> String
createPuppeteer localStorageJs =
  unlines $ map replaceLine (lines clientJsTemplate)
  where
    -- Replace any line containing only __LOCAL_STORAGE__ (possibly with whitespace)
    replaceLine line
      | trim line == "__LOCAL_STORAGE__" = localStorageJs
      | otherwise = line
    trim = unwords . words

-- | Example localStorage JavaScript block
exampleLocalStorageJs :: String
exampleLocalStorageJs = unlines
  [ "await page.goto('about:blank');"
  , "await page.evaluate(() => {"
  , "  localStorage.setItem('user', 'branko');"
  , "  localStorage.setItem('theme', 'dark');"
  , "});"
  ]

-- | Write the generated Puppeteer client code to a file and return the filename
-- Accepts a Map of localStorage key-value pairs
writePuppeteerClient :: Map.Map String String -> IO FilePath
writePuppeteerClient localStorageMap = do
  let localStorageJs = generateLocalStorageJs localStorageMap
      filename = "client.generated.js"
      code = createPuppeteer localStorageJs
  writeFile filename code
  return filename

-- | Helper to generate JS code for localStorage from a Map
generateLocalStorageJs :: Map.Map String String -> String
generateLocalStorageJs m = unlines $
  [ "await page.goto('about:blank');"
  , "await page.evaluate(() => {"
  ]
  ++ map (\(k,v) -> "  localStorage.setItem('" ++ escapeJs k ++ "', '" ++ escapeJs v ++ "');") (Map.toList m)
  ++ ["});"]

-- | Escape single quotes and backslashes for JS string literals
escapeJs :: String -> String
escapeJs = concatMap escapeChar
  where
    escapeChar '\'' = "\\'"
    escapeChar '\\' = "\\\\"
    escapeChar c    = [c]

-- | Simple test for the escapeJs function
testEscapeJs :: IO ()
testEscapeJs = do
  let testStrings = ["plain", "O'Reilly", "back\\slash", "mix'ed\\up"]
  mapM_ 
   (\s -> putStrLn $ "Original: " ++ s ++ " | Escaped: " ++ escapeJs s) 
   testStrings

-- | Test for generateLocalStorageJs to show escaping in JS code
testGenerateLocalStorageJs :: IO ()
testGenerateLocalStorageJs = do
    let 
     testMap = 
      Map.fromList
        [ ("plain", "simple")
            , ("O'\"Reilly", "quote's")
            , ("back\\slash", "slash\\value")
            , ("mix'ed\\up", "mix'ed\\up\"")
        ]
    putStrLn $ generateLocalStorageJs testMap

-- | Test for createPuppeteer to verify LOCAL_STORAGE replacement
testCreatePuppeteer :: IO ()
testCreatePuppeteer = do
  let injectedJs = "// injected localStorage JS block"
      clientCode = createPuppeteer injectedJs
  putStrLn "=== Puppeteer client.js with injected localStorage ==="
  putStrLn clientCode