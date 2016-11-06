{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}

import Control.Exception (displayException, fromException, try)

import Test.HUnit

import GSI.Util (Pos(Pos), gsfatal, fmtPos)
import GSI.Value (GSValue(..), gsundefined_w, gsapply_w, gstoplevelclosure_w, gsvCode)
import GSI.Result (GSError(..), GSResult(..), GSException(..), stCode)
import GSI.Eval (eval, evalSync)
import GSI.ByteCode (GSBCO, gsbcundefined_w)
import GSI.Thread (createThread, execMainThread)

import qualified GSI.Value as GSV

getThunk v = case v of
    GSThunk th -> return th
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
            GSV.GSError (GSErrUnimpl pos) -> assertEqual "The returned error has the right location" pos (Pos file line)
            _ -> assertFailure $ "Got " ++ gsvCode v ++ "; expected error"
    ,
    TestCase $ do
        let file = "test-file.gs"
        th <- getThunk =<< gsapply_w (Pos file 1) (gstoplevelclosure_w (Pos file 2) $ (\ (x :: GSValue) -> gsbcundefined_w (Pos file 3))) [gsundefined_w (Pos file 4)]
        st <- eval th
        case st of
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
