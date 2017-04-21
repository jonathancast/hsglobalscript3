{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, catch, displayException)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import Component.Monad (mvarContents)

import GSI.Util (fmtPos, gshere)
import GSI.Value (GSValue, gsapply, gsundefined_value)
import GSI.ThreadType (ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import GSI.Functions (gslist, gsstring)
import GSI.Env (GSEnvArgs(..))
import GSI.Main (gsmain)

main = do
    as <- newMVar . GSEnvArgs . $gslist . map $gsstring =<< getArgs
    t <- createThread $gshere (testGSIThreadData TestGSIThread{ envArgs = as }) =<< $gsapply gsmain [gstyc, gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gstyc = $gsundefined_value

gsrun = $gsundefined_value

data TestGSIThread = TestGSIThread{
    envArgs :: MVar GSEnvArgs
  }

testGSIThreadData :: TestGSIThread -> ThreadData
testGSIThreadData d = ThreadData{
    component = fetchThreadDataComponent testGSIThreadComponents d,
    threadTypeName = fmtPos $gshere "TestGSIThread"
  }

testGSIThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents
