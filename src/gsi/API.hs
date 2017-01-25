{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallExpr, apiImplementationFailure, apiImplementationFailure_w) where

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Value (GSValue(..), GSExpr(..), gsvCode, exprCode)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread, ThreadException(..))

apiCall pos0 (GSImplementationFailure pos1 e) t = throwIO $ TEImplementationFailure pos1 e
apiCall pos (GSError err) t = throwIO $ TEError err
apiCall pos (GSThunk th) t = do
    v <- evalSync th
    apiCall pos v t
apiCall pos0 (GSImp pos1 a) t = a t
apiCall pos v t = do
    throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallExpr :: Pos -> GSExpr -> Thread -> IO GSValue
apiCallExpr pos (GSExpr e) t = do
    v <- e [] [StackTrace pos []]
    apiCall pos v t
apiCallExpr pos0 (GSExprVar pos1 v) t = apiCall pos0 v t
apiCallExpr pos e t = throwIO $ TEImplementationFailure $gshere $ "apiCallExpr " ++ exprCode e ++ " next"

apiImplementationFailure = varE 'apiImplementationFailure_w `appE` gshere

apiImplementationFailure_w pos err = throwIO $ TEImplementationFailure pos err
