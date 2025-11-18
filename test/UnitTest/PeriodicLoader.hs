module UnitTest.PeriodicLoader where 

import Data.Time (UTCTime, addUTCTime)

import PeriodicLoader

runTests :: IO ()
runTests = sequence_ [testYoungerThan]

testYoungerThan :: IO ()
testYoungerThan = do
  let now = read "2023-01-01 12:00:00 UTC" :: UTCTime
      threshold = Minutes 10
      -- Within examples
      within5 = addUTCTime (-300) now -- 5 minutes ago
      within9_59 = addUTCTime (-599) now -- 9:59 minutes ago
      within0 = now -- exactly now
      -- Edge case: exactly at threshold
      atThreshold = addUTCTime (-600) now -- 10 minutes ago
      -- Outside examples
      outside10_01 = addUTCTime (-601) now -- 10:01 minutes ago
      outside20 = addUTCTime (-1200) now -- 20 minutes ago
  putStrLn "Testing youngerThan..."
  if youngerThan now threshold within5
    then putStrLn "Pass: 5 min ago is younger"
    else putStrLn "Fail: 5 min ago should be younger"
  if youngerThan now threshold within9_59
    then putStrLn "Pass: 9:59 min ago is younger"
    else putStrLn "Fail: 9:59 min ago should be younger"
  if youngerThan now threshold within0
    then putStrLn "Pass: now is younger"
    else putStrLn "Fail: now should be younger"
  if not (youngerThan now threshold atThreshold)
    then putStrLn "Pass: exactly at threshold is not younger"
    else putStrLn "Fail: exactly at threshold should not be younger"
  if not (youngerThan now threshold outside10_01)
    then putStrLn "Pass: 10:01 min ago is not younger"
    else putStrLn "Fail: 10:01 min ago should not be younger"
  if not (youngerThan now threshold outside20)
    then putStrLn "Pass: 20 min ago is not younger"
    else putStrLn "Fail: 20 min ago should not be younger"