{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Test.HUnit

import GSI.Util (Pos(Pos), fmtPos)
import GSI.Value (GSValue(GSUndefined))
import GSI.Result (GSError(..), GSResult(..), stCode)
import GSI.Eval (eval)
import GSI.Thread (createThread)

main = runTestTT $ TestList $ [
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        st <- eval $ GSUndefined (Pos file line)
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected error"
  ,
  TestCase $ do
      let file = "test-file.gs"
      let line = 1
      t <- createThread $ GSUndefined (Pos file line)
      return ()
  ]
