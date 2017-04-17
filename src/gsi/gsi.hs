{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, catch, displayException)

import Component.Monad (mvarContents)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Util (gsfatal, fmtPos, gshere)
import GSI.Value (gslambda, gsapply, gsundefined_value, gsae)
import GSI.ThreadType (ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import GSI.Functions (gslist, gsstring)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcundefined, gsbcimpfor, gsbcimpbind, gsbcimpbody)
import GSI.Env (GSEnvArgs(..))
import GSI.GSI (gsicreateThread)
import GSI.Main (gsmain)

main = do
    as <- newMVar . GSEnvArgs . $gslist . map $gsstring =<< getArgs
    t <- createThread $gshere GSIThread{ envArgs = as } =<< $gsapply gsmain [gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsrun = $gslambda $ \ prog -> $gsbcarg $ \ args -> $gsbcimpfor $ do
    t <- $gsbcimpbind $ $gsae $ $gsbcapply gsicreateThread [ $gsae $ $gsbcundefined, $gsae $ $gsbcundefined ] -- hs{GSIThread{ envArgs = args }, gs{$gsbcapply gsmain [prog]}
    $gsbcimpbody $ $gsae $ $gsbcundefined

data GSIThread = GSIThread{
    envArgs :: MVar GSEnvArgs
  }

instance ThreadData GSIThread where
    component d = fetchThreadDataComponent gsiThreadComponents d
    threadTypeName _ = fmtPos $gshere "GSIThread"

gsiThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents
