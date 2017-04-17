{-# LANGUAGE TemplateHaskell #-}
module GSI.GSI (gsigsundefined, gsicreateThread) where

import GSI.Util (Pos)
import GSI.Value (GSValue, gsimpprim, gsundefined_value)
import GSI.ThreadType (Thread)
import API (apiImplementationFailure)

gsigsundefined = $gsundefined_value

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t td v = do
    $apiImplementationFailure $ "gsiprimcreateThread next"
