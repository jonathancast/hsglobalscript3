{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), gshere, gsfatal, fmtPos)
import GSI.Error (GSError(..), GSException(..))
import GSI.Value (GSValue(..), gsundefined_w, gsapply_w, gstoplevelclosure_w, gsclosure_w, gsvCode)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)
import GSI.ByteCode (GSBCO, gsbcundefined_w, gsbcbody_w)
import GSI.Thread (createThread, execMainThread)

getThunk v = case v of
    GSThunk th -> return th
    GSImplementationFailure pos err -> do assertFailure $ fmtPos pos err; $gsfatal "oops"
    _ -> do assertFailure $ "Got " ++ gsvCode v ++ " from gsapply; expected thunk"; $gsfatal "oops"

main = runTestTT $ TestList $ [
    -- §section Expressions
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        th <- getThunk =<< gsapply_w (Pos file line) (gsundefined_w (Pos file line)) []
        st <- eval th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        v <- evalSync =<< getThunk =<< gsapply_w (Pos file line) (gsundefined_w (Pos file line)) []
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< (gsclosure_w (Pos file 2) $ gsbcundefined_w (Pos file 3))
        st <- eval th
        case st of
            GSIndirection v -> case v of
                GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
                _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected stack"
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< gsapply_w (Pos file 1) (gstoplevelclosure_w (Pos file 2) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3))) [gsundefined_w (Pos file 4)]
        st <- eval th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- evalSync =<< getThunk =<< gsapply_w (Pos file 1) (gstoplevelclosure_w (Pos file 2) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3))) [gsundefined_w (Pos file 4)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file 3)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- gsclosure_w (Pos file 1) $ gsbcbody_w (Pos file 2) $ gsbcundefined_w (Pos file 3)
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
            GSClosure pos gsbc -> assertEqual "The returned closure has the right position" pos (Pos file 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++"; expected closure"
    ,
    -- §section Threads
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere $ gsundefined_w (Pos file line)
        return ()
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere $ gsundefined_w (Pos file line)
        mb <- try $ execMainThread t
        case mb of
            Right _ -> assertFailure "execMainThread should throw an exception when the thread's code is undefined"
            Left e -> case fromException e of
                Just (GSExcUndefined pos) -> assertEqual "execMainThread should and exception with the right source location" pos (Pos file line)
                _ -> assertFailure $ "execMainThread should throw a GSExcUndefined error, but instead threw " ++ displayException e
  ]
