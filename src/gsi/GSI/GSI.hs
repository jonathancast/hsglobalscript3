{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module GSI.GSI (gsigsundefined, gsicreateThread, gsigsiThreadData) where

import Control.Concurrent.MVar (newMVar)

import GSI.Util (Pos)
import GSI.Value (GSValue, GSExternal, gsimpprim, gsundefined_value)
import GSI.ThreadType (Thread, ThreadData)
import API (apiImplementationFailure)
import GSI.Functions (gsapiEvalExternal)
import GSI.Env (GSEnvArgs(..))

gsigsundefined = $gsundefined_value

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t tdv vv = do
    td <- gsapiEvalExternal pos tdv :: IO GSIThreadData
    $apiImplementationFailure $ "gsiprimcreateThread next"

gsigsiThreadData :: GSValue
gsigsiThreadData = $gsimpprim gsiprimgsiThreadData

gsiprimgsiThreadData :: Pos -> Thread -> GSValue -> IO GSValue
gsiprimgsiThreadData pos t args = do
    as <- newMVar $ GSEnvArgs $ args
    $apiImplementationFailure $ "gsiprimgsiThreadData next"

data GSIThreadData = forall d. ThreadData d => GSIThreadData d

instance GSExternal GSIThreadData
