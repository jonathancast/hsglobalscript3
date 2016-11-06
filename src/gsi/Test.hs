{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), gsfatal, fmtPos)
import GSI.Value (GSValue(GSThunk), gsundefined_w, gsapply_w, gstoplevelclosure_w, gsvCode)
import GSI.Result (GSError(..), GSResult(..), GSException(..), stCode)
import GSI.Eval (eval, evalSync)
import GSI.ByteCode (GSBCO, gsbcundefined_w)
import GSI.Thread (createThread, execMainThread)

main = runTestTT $ TestList $ [
    -- §section Expressions
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        v <- gsapply_w (Pos file line) (gsundefined_w (Pos file line)) []
        th <- case v of
            GSThunk th -> return th
            _ -> do assertFailure $ "Got " ++ gsvCode v ++ " from gsapply; expected thunk"; $gsfatal "oops"
        st <- eval th
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        st <- evalSync =<< gsapply_w (Pos file line) (gsundefined_w (Pos file line)) []
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- gsapply_w (Pos file 1) (gstoplevelclosure_w (Pos file 2) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3))) [gsundefined_w (Pos file 4)]
        th <- case v of
            GSThunk th -> return th
            _ -> do assertFailure $ "Got " ++ gsvCode v ++ " from gsapply; expected thunk"; $gsfatal "oops"
        st <- eval th
        case st of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    -- §section Threads
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $ gsundefined_w (Pos file line)
        return ()
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $ gsundefined_w (Pos file line)
        mb <- try $ execMainThread t
        case mb of
            Right _ -> assertFailure "execMainThread should throw an exception when the thread's code is undefined"
            Left e -> case fromException e of
                Just (GSExcUndefined pos) -> assertEqual "execMainThread should and exception with the right source location" pos (Pos file line)
                _ -> assertFailure $ "execMainThread should throw a GSExcUndefined error, but instead threw " ++ displayException e
  ]
