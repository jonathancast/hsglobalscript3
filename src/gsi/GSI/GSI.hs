{-# LANGUAGE TemplateHaskell, ExistentialQuantification #-}
module GSI.GSI (gsigsinject, gsigsapply, gsigsundefined, gsicreateThread, GSIThread(..), gsigsiThreadData) where

import Control.Concurrent.MVar (MVar, newMVar)

import Component.Monad (mvarContents)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.Value (GSValue(..), GSExternal(..), gslambda, gsimpprim, gsundefined_value)
import GSI.ThreadType (Thread, ThreadData(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import API (apiImplementationFailure)
import GSI.Functions (gsapiEvalExternal, gsapiEvalList)
import GSI.ByteCode (gsbcexternal, gsbcundefined)
import GSI.Env (GSEnvArgs(..))

gsigsinject = $gslambda $ \ v -> $gsbcexternal (GSIGSValue v)

gsigsapply :: GSValue
gsigsapply = $gsimpprim gsiprimgsapply

gsiprimgsapply :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimgsapply pos t fv asv = do
    GSIGSValue f <- gsapiEvalExternal pos fv
    asgsi <- gsapiEvalList pos asv
    $apiImplementationFailure $ "gsiprimgsapply next"

gsigsundefined = $gsundefined_value

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t tdv vv = do
    td <- gsapiEvalExternal pos tdv :: IO GSIThreadData
    v <- gsapiEvalExternal pos vv :: IO GSIGSValue
    $apiImplementationFailure $ "gsiprimcreateThread next"

gsigsiThreadData :: GSValue
gsigsiThreadData = $gsimpprim gsiprimgsiThreadData

gsiprimgsiThreadData :: Pos -> Thread -> GSValue -> IO GSValue
gsiprimgsiThreadData pos t args = do
    as <- newMVar $ GSEnvArgs $ args
    gsiprimthreadData pos t GSIThread{ envArgs = as }

gsiprimthreadData :: ThreadData d => Pos -> Thread -> d -> IO GSValue
gsiprimthreadData pos t td = do
    return $ GSExternal $ toExternal $ GSIThreadData td

-- ↓ This wraps up an §emph{arbitrary} §hs{ThreadData} value for then including into a GSValue
data GSIThreadData = forall d. ThreadData d => GSIThreadData d

instance GSExternal GSIThreadData

-- ↓ This is the §hs{ThreadData} value for a §gs{gsi.m} thread specifically
data GSIThread = GSIThread{
    envArgs :: MVar GSEnvArgs
  }

instance ThreadData GSIThread where
    component d = fetchThreadDataComponent gsiThreadComponents d
    threadTypeName _ = fmtPos $gshere "GSIThread"

gsiThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents

newtype GSIGSValue = GSIGSValue GSValue

instance GSExternal GSIGSValue
