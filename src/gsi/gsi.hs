{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, catch, displayException)

import Component.Monad (mvarContents)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Util (gsfatal, fmtPos, gshere)
import GSI.Value (gsapply, gsundefined)
import GSI.ThreadType (ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import GSI.Functions (gslist, gsstring)
import GSI.Env (GSEnvArgs(..))
import GSI.Main (gsmain)

main = do
    as <- newMVar . GSEnvArgs . $gslist . map $gsstring =<< getArgs
    t <- createThread $gshere GSIThread{ envArgs = as } =<< $gsapply gsmain [gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsrun = $gsundefined

data GSIThread = GSIThread{
    envArgs :: MVar GSEnvArgs
  }

instance ThreadData GSIThread where
    component d = fetchThreadDataComponent gsiThreadComponents d
    threadTypeName _ = fmtPos $gshere "GSIThread"

gsiThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents
