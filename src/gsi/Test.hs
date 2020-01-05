{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), StackTrace(..), gshere, gsfatal, fmtPos)
import GSI.Error (GSError(..), GSException(..))
import GSI.Value (GSValue(..), GSArg(..), GSExternal(..), gsundefined_value_w, gsapply_w, gslambda_w, gsthunk_w, gsargexpr_w, gsimpfor_w, whichExternal, gsvCode)
import GSI.Eval (GSResult(..), eval, evalSync, stCode)
import GSI.ByteCode (gsbcwithhere_w, gsbcrehere_w, gsbcforce_w, gsbcundefined, gsbcenter_w, gsbcrecord_w, gsbcimpbody_w)
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
        th <- getThunk =<< gsapply_w (Pos file line col) (gsundefined_value_w (Pos file line col)) []
        st <- eval [] th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        let col = 1
        v <- evalSync [] =<< getThunk =<< gsapply_w (Pos file line col) (gsundefined_value_w (Pos file line col)) []
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file line col)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< (gsthunk_w (Pos file 2 1) $ gsbcundefined (StackTrace (Pos file 3 1) []))
        st <- eval [] th
        case st of
            GSIndirection v -> case v of
                GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
                _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected stack"
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        fn <- gsthunk_w (Pos file 2 1) $ gsbcundefined (StackTrace (Pos file 3 1) [])
        st <- eval [] =<< getThunk =<< gsapply_w (Pos file 1 1) fn [gsundefined_value_w (Pos file 4 1)]
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< gsapply_w (Pos file 1 1) (gslambda_w (Pos file 2 1) $ (\ (x :: GSValue) -> gsbcundefined (StackTrace (Pos file 3 1) []))) [gsundefined_value_w (Pos file 4 1)]
        st <- eval [] th
        case st of
            GSStack _ -> return ()
            _ -> assertFailure $ "Got " ++ stCode st ++ "; expected stack"
    ,
    TestCase $ do
        let file = "test-file.gs"
        fn <- gsthunk_w (Pos file 2 1) $ gsbcundefined (StackTrace (Pos file 3 1) [])
        v <- evalSync [] =<< getThunk =<< gsapply_w (Pos file 1 1) fn [gsundefined_value_w (Pos file 4 1)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file 3 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- evalSync [] =<< getThunk =<< gsapply_w (Pos file 1 1) (gslambda_w (Pos file 2 1) $ (\ (x :: GSValue) -> gsbcundefined (StackTrace (Pos file 3 1) []))) [gsundefined_value_w (Pos file 4 1)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos $ ": " ++ msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file 3 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        let v = gsimpfor_w (Pos file 1 1) $ gsbcimpbody_w (Pos file 2 1) $ gsargexpr_w (Pos file 3 1) $ gsbcundefined (StackTrace (Pos file 4 1) [])
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
            GSClosure [StackTrace pos _] bco -> assertEqual "The returned closure has the right position" pos (Pos file 1 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++"; expected closure"
    ,
    TestCase $ do
        let file = "test-file.gs"
        v <- evalSync [] =<< getThunk =<< gsapply_w (Pos file 1 1) (gslambda_w (Pos file 2 1) $ (\ (x :: GSValue) -> gsbcrehere_w (Pos file 3 1) $ gsbcundefined (StackTrace [] (Pos file 4 1)))) [gsundefined_value_w (Pos file 4 1)]
        case v of
            GSImplementationFailure pos msg -> assertFailure $ fmtPos pos msg
            GSError (GSErrUnimpl (StackTrace pos _)) -> assertEqual "The returned error has the right location" pos (Pos file 3 1)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    -- §section Threads
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere (gsundefined_value_w (Pos file line 1)) Nothing
        return ()
    ,
    TestCase $ do
        let file = "test-file.gs"
        let line = 1
        t <- createThread $gshere (gsundefined_value_w (Pos file line 1)) Nothing
        mb <- try $ execMainThread t
        case mb of
            Right _ -> assertFailure "execMainThread should throw an exception when the thread's code is undefined"
            Left e -> case fromException e of
                Just (GSExcError (GSErrUnimpl (StackTrace pos _))) -> assertEqual "execMainThread should and exception with the right source location" pos (Pos file line 1)
                _ -> assertFailure $ "execMainThread should throw a GSExcUndefined error, but instead threw " ++ displayException e
    ,
    TestCase $ do
        let file = "test-file.gs"
        t <- gsthunk_w (Pos file 2 1) $ gsbcwithhere_w (Pos file 3 5) (\ posv -> gsbcenter_w (Pos file 3 5) posv)
        v <- evalSync [StackTrace (Pos file 1 1) []] =<< getThunk t
        case v of
            GSExternal e -> case fromExternal e of
                Just st -> assertEqual "The stack trace should include the gsthunk_w" (StackTrace (Pos file 3 5) [StackTrace (Pos file 2 1) [], StackTrace (Pos file 1 1) []]) st
                _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ whichExternal e
            _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ gsvCode v
    ,
    TestCase $ do
        let file = "test-file.gs"
        t <- gsthunk_w (Pos file 2 1) $
            gsbcforce_w (Pos file 3 1)
                (GSArgExpr (Pos file 3 5) (gsbcwithhere_w (Pos file 3 5) (\ posv -> gsbcenter_w (Pos file 3 5) posv)))
                (\ posv -> gsbcenter_w (Pos file 3 10) posv)
        v <- evalSync [StackTrace (Pos file 1 1) []] =<< getThunk t
        case v of
            GSExternal e -> case fromExternal e of
                Just st -> assertEqual "The stack trace should include the force call" st $
                    StackTrace (Pos file 3 5) [StackTrace (Pos file 3 1) [StackTrace (Pos file 2 1) [], StackTrace (Pos file 1 1) []]]
                _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ whichExternal e
            _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ gsvCode v
    ,
    TestCase $ do
        let file = "test-file.gs"
        t <- gsthunk_w (Pos file 2 1) $
            gsbcforce_w (Pos file 3 1)
                (GSArgExpr (Pos file 3 5) (gsbcrecord_w (Pos file 3 5) []))
                (\ _ -> gsbcwithhere_w (Pos file 3 10) (\ posv -> gsbcenter_w (Pos file 3 10) posv))
        v <- evalSync [StackTrace (Pos file 1 1) []] =<< getThunk t
        case v of
            GSExternal e -> case fromExternal e of
                Just st -> assertEqual "The stack trace should not include the force call" (StackTrace (Pos file 3 10) [StackTrace (Pos file 2 1) [], StackTrace (Pos file 1 1) []]) st
                _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ whichExternal e
            _ -> assertFailure $ "We should have gotten a stack tracce, but got " ++ gsvCode v
  ]
