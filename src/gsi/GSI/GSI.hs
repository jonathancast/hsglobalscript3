{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module GSI.GSI (gsigsundefined, gsicreateThread) where

import GSI.Util (Pos)
import GSI.Value (GSValue, GSExternal, gsimpprim, gsundefined_value)
import GSI.ThreadType (Thread, ThreadData)
import API (apiImplementationFailure)
import GSI.Functions (gsapiEvalExternal)

gsigsundefined = $gsundefined_value

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t tdv vv = do
    td <- gsapiEvalExternal pos tdv :: IO GSIThreadData
    $apiImplementationFailure $ "gsiprimcreateThread next"

data GSIThreadData = forall d. ThreadData d => GSIThreadData d

instance GSExternal GSIThreadData
