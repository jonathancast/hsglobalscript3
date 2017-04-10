{-# LANGUAGE TemplateHaskell #-}

import Control.Exception (SomeException, catch, displayException)

import System.Exit (ExitCode(..), exitWith)
import System.IO (hPutStrLn, stderr)

import GSI.Util (gsfatal)

main = do
    $gsfatal "main next"
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages