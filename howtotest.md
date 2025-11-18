start relay with the data you want it to return
 - should receive filter F
 - and return results R

oldModel
send action
check that newwmodel is what you expect

you need to be able to trigger actions on Miso application
you need to be able to state what requests a relay should receive, 

based on the events in the relayDB and the actions called on Miso app,
you need to be able to state how ding-dong Miso model should have changed

a test should look like:

 you provide the full relayDB - some list of events
 you issue an action on Miso app
 you observe what other actions are triggered/how the model has changed after handling those actions
    assertRequestsReceived :: [Request] -- assert that particular requests were requested in some particular sequence
    assertActionSequence :: [Action] -> Bool -- assert that particular actions were called in particular sequence
    assertAfter :: Action -> (Model -> Bool) -- assert that after action the model looks like you expect it to

