{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), StackTrace(..), gshere, gsfatal, fmtPos)
import GSI.Error (GSError(..), GSException(..))
import GSI.Value (GSValue(..), gsundefined_w, gsapply_w, gslambda_w, gsthunk_w, gsimpfor_w, gsvCode)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)
import GSI.ByteCode (gsbcundefined_w, gsbcimpbody_w)
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
        let col = 1
        th <- getThunk =<< gsapply_w (Pos file line col) (gsundefined_w (Pos file line col)) []
        st <- eval th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        let col = 1
        v <- evalSync =<< getThunk =<< gsapply_w (Pos file line col) (gsundefined_w (Pos file line col)) []
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file line col)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< (gsthunk_w (Pos file 2 1) $ gsbcundefined_w (Pos file 3 1))
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
        fn <- gsthunk_w (Pos file 2 1) $ gsbcundefined_w (Pos file 3 1)
        st <- eval =<< getThunk =<< gsapply_w (Pos file 1 1) fn [gsundefined_w (Pos file 4 1)]
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< gsapply_w (Pos file 1 1) (gslambda_w (Pos file 2 1) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3 1))) [gsundefined_w (Pos file 4 1)]
        st <- eval th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        fn <- gsthunk_w (Pos file 2 1) $ gsbcundefined_w (Pos file 3 1)
        v <- evalSync =<< getThunk =<< gsapply_w (Pos file 1 1) fn [gsundefined_w (Pos file 4 1)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file 3 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- evalSync =<< getThunk =<< gsapply_w (Pos file 1 1) (gslambda_w (Pos file 2 1) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3 1))) [gsundefined_w (Pos file 4 1)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file 3 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let v = gsimpfor_w (Pos file 1 1) $ gsbcimpbody_w (Pos file 2 1) $ gsbcundefined_w (Pos file 3 1)
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
            GSClosure [StackTrace pos _] bco -> assertEqual "The returned closure has the right position" pos (Pos file 1 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++"; expected closure"
    ,
    -- §section Threads
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere () $ gsundefined_w (Pos file line 1)
        return ()
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere () $ gsundefined_w (Pos file line 1)
        mb <- try $ execMainThread t
        case mb of
            Right _ -> assertFailure "execMainThread should throw an exception when the thread's code is undefined"
            Left e -> case fromException e of
                Just (GSExcUndefined (StackTrace pos _)) -> assertEqual "execMainThread should and exception with the right source location" pos (Pos file line 1)
                _ -> assertFailure $ "execMainThread should throw a GSExcUndefined error, but instead threw " ++ displayException e
  ]
