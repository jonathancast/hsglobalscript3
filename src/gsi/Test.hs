{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), gsfatal, fmtPos)
import GSI.Value (GSValue(GSUndefined), gsapply_w, gstoplevelclosure_w)
import GSI.Result (GSError(..), GSResult(..), GSException(..), stCode)
import GSI.Eval (eval, evalSync)
import GSI.ByteCode (GSBCO)
import GSI.Thread (createThread, execMainThread)

main = runTestTT $ TestList $ [
    -- §section Expressions
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
        st <- eval =<< gsapply_w (Pos file line) (GSUndefined (Pos file line)) []
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        st <- evalSync =<< gsapply_w (Pos file line) (GSUndefined (Pos file line)) []
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        st <- eval $ gstoplevelclosure_w (Pos file line) $ ($gsfatal "Evalling a known closure shouldn't touch the function" :: GSValue () -> GSBCO ())
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            GSWHNF -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    -- §section Threads
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $ GSUndefined (Pos file line)
        return ()
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $ GSUndefined (Pos file line)
        mb <- try $ execMainThread t
        case mb of
            Right _ -> assertFailure "execMainThread should throw an exception when the thread's code is undefined"
            Left e -> case fromException e of
                Just (GSExcUndefined pos) -> assertEqual "execMainThread should and exception with the right source location" pos (Pos file line)
                _ -> assertFailure $ "execMainThread should throw a GSExcUndefined error, but instead threw " ++ displayException e
  ]
