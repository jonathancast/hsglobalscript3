{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, catch, displayException)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import Component.Monad (mvarContents)

import GSI.Util (fmtPos, gshere)
import GSI.Value (GSValue, gsapply, gsundefined)
import GSI.ThreadType (ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import GSI.Functions (gslist, gsstring)
import GSI.Env (GSEnvArgs(..))
import GSI.Main (gsmain)

main = do
    as <- newMVar . GSEnvArgs . $gslist . map $gsstring =<< getArgs
    t <- createThread $gshere TestGSIThread{ envArgs = as } =<< $gsapply gsmain [gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsrun = $gsundefined

data TestGSIThread = TestGSIThread{
    envArgs :: MVar GSEnvArgs
  }

instance ThreadData TestGSIThread where
    component d = fetchThreadDataComponent testGSIThreadComponents d
    threadTypeName _ = fmtPos $gshere "TestGSIThread"

testGSIThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents
