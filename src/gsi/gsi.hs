{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent.MVar (newMVar)
import Control.Exception (SomeException, catch, displayException)

import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Util (gsfatal, gshere)
import GSI.Value (gslambda, gsapply, gsundefined_value, gsav, gsae)
import GSI.Thread (createThread, execMainThread)
import GSI.Functions (gslist, gsstring)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcundefined, gsbcimpfor, gsbcimpbind, gsbcimpbody)
import GSI.Env (GSEnvArgs(..))
import GSI.GSI (gsicreateThread, GSIThread(..), gsigsiThreadData)
import GSI.Main (gsmain)

main = do
    as <- newMVar . GSEnvArgs . $gslist . map $gsstring =<< getArgs
    t <- createThread $gshere GSIThread{ envArgs = as } =<< $gsapply gsmain [gsrun]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsrun = $gslambda $ \ prog -> $gsbcarg $ \ args -> $gsbcimpfor $ do
    td <- $gsbcimpbind $ $gsae $ $gsbcapply gsigsiThreadData [ $gsav args ]
    t <- $gsbcimpbind $ $gsae $ $gsbcapply gsicreateThread [ $gsav td, $gsae $ $gsbcundefined ] -- gs{$gsbcapply gsmain [prog]}
    $gsbcimpbody $ $gsae $ $gsbcundefined
