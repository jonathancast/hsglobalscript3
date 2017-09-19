{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallExpr, apiImplementationFailure, apiImplementationFailure_w) where

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), gsvCode, bcoCode)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread, ThreadException(..))

apiCall :: Pos -> GSValue -> Thread -> IO GSValue
apiCall pos0 (GSImplementationFailure pos1 e) t = throwIO $ TEImplementationFailure pos1 e
apiCall pos (GSError err) t = throwIO $ TEError err
apiCall pos (GSThunk th) t = do
    v <- evalSync [StackTrace pos []] th
    apiCall pos v t
apiCall pos0 (GSClosure cs bco) t = case bco of
    GSImp a -> a t
    _ -> throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is (GSClosure cs " ++ bcoCode bco ++ ")) next"
apiCall pos v t = do
    throwIO $ TEImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallExpr :: Pos -> GSExpr -> Thread -> IO GSValue
apiCallExpr pos (GSExpr e) t = do
    v <- e [StackTrace pos []] GSExprCont{ gsreturn = return, gsthrow = return }
    apiCall pos v t

apiImplementationFailure = varE 'apiImplementationFailure_w `appE` gshere

apiImplementationFailure_w pos err = throwIO $ TEImplementationFailure pos err
