{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallBCO) where

import Control.Exception (throwIO)

import GSI.Util (Pos, gshere)
import GSI.Value (GSValue(..), GSBCO(..), gsvCode, bcoCode)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread, ThreadException(..))

apiCall pos0 (GSImplementationFailure pos1 e) t = throwIO $ TEImplementationFailure pos1 e
apiCall pos (GSError err) t = throwIO $ TEError err
apiCall pos (GSThunk th) t = do
    v <- evalSync th
    apiCall pos v t
apiCall pos0 (GSClosure pos1 bco) t = case bco of
    GSBCOImp a -> a t
    _ -> throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ bcoCode bco ++ ") next"
apiCall pos v t = do
    throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallBCO :: Pos -> GSBCO -> Thread -> IO GSValue
apiCallBCO pos (GSBCOExpr e) t = do
    v <- e
    apiCall pos v t
apiCallBCO pos bco t = throwIO $ TEImplementationFailure $gshere $ "apiCallBCO " ++ bcoCode bco ++ " next"
