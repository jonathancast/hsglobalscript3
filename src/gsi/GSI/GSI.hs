{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module GSI.GSI (gsigsinject, gsigsapply, gsigsundefined, gsicreateThread, GSIThread(..), gsiThreadData, gsigsiThreadData) where

import Control.Concurrent.MVar (MVar, newMVar)

import Component.Monad (mvarContents)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.Value (GSValue(..), GSExternal(..), gslambda, gsimpprim, gsundefined_value, gsundefined_value_w, gsav)
import GSI.ThreadType (Thread, ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread)
import API (apiImplementationFailure)
import GSI.Functions (gsapiEvalExternal, gsapiEvalList)
import GSI.ByteCode (gsbcexternal, gsbcundefined)
import GSI.Env (GSEnvArgs(..))
import GSI.StdLib (gsbcevalpos)

gsigsinject = $gslambda $ \ v -> $gsbcexternal (GSIGSValue v)

gsigsapply :: GSValue
gsigsapply = $gsimpprim gsiprimgsapply

gsiprimgsapply :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimgsapply pos t fv asv = do
    GSIGSValue f <- gsapiEvalExternal pos fv
    as <- gsapiEvalList pos asv >>= mapM (\ av -> gsapiEvalExternal pos av >>= \ (GSIGSValue a) -> return a)
    $apiImplementationFailure $ "gsiprimgsapply next"

gsigsundefined = $gslambda $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal $ GSIGSValue $ gsundefined_value_w pos

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t tdv vv = do
    td <- gsapiEvalExternal pos tdv
    GSIGSValue v <- gsapiEvalExternal pos vv
    t <- createThread pos td v
    return $ GSExternal $ toExternal t

gsigsiThreadData :: GSValue
gsigsiThreadData = $gsimpprim gsiprimgsiThreadData

gsiprimgsiThreadData :: Pos -> Thread -> GSValue -> IO GSValue
gsiprimgsiThreadData pos t args = do
    as <- newMVar $ GSEnvArgs $ args
    gsiprimthreadData pos t (gsiThreadData GSIThread{ envArgs = as })

gsiprimthreadData :: Pos -> Thread -> ThreadData -> IO GSValue
gsiprimthreadData pos t td = do
    return $ GSExternal $ toExternal td

data GSIThread = GSIThread{
    envArgs :: MVar GSEnvArgs
  }

gsiThreadData :: GSIThread -> ThreadData
gsiThreadData d = ThreadData{
    component = fetchThreadDataComponent gsiThreadComponents d,
    threadTypeName = fmtPos $gshere "GSIThread"
  }

gsiThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents

newtype GSIGSValue = GSIGSValue GSValue

instance GSExternal GSIGSValue
