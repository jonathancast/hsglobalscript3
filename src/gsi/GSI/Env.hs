{-# LANGUAGE TemplateHaskell #-}
module GSI.Env (gsenvGetArgs) where

import System.IO.Unsafe (unsafePerformIO)

import GSI.Util (Pos)
import GSI.Value (GSValue, GSBCO, gsclosure)
import GSI.Thread (Thread)
import GSI.ByteCode (gsbcimpprim)
import API (apiImplementationFailure)

gsenvGetArgs = unsafePerformIO $ $gsclosure ($gsbcimpprim gsprimenvGetArgs :: GSBCO)

gsprimenvGetArgs :: Pos -> Thread -> IO GSValue
gsprimenvGetArgs pos t = $apiImplementationFailure "gsprimenvGetArgs next"
