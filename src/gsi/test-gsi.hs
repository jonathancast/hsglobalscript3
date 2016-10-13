{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (SomeException, catch, displayException)

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Value (gsundefined)
import GSI.Thread (createThread, execMainThread)

main = do
    t <- createThread $gsundefined -- =<< $gsapply gsrun [gsmain]
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages
