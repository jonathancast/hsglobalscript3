{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (SomeException, catch, displayException)

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Util (fmtPos, gshere)
import GSI.Value (gsapply, gsundefined)
import GSI.ThreadType (ThreadData(..), fetchThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import GSI.Main (gsmain)

main = do
    t <- createThread $gshere TestGSIThread =<< $gsapply gsmain [gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsrun = $gsundefined

data TestGSIThread = TestGSIThread{
  }

instance ThreadData TestGSIThread where
    component d = fetchThreadDataComponent testGSIThreadComponents d
    threadTypeName _ = fmtPos $gshere "TestGSIThread"

testGSIThreadComponents =
    emptyThreadDataComponents
