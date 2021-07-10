{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallExpr, apiImplementationFailure, apiImplementationFailure_w) where

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSEvalState(..), GSExprCont(..), GSException(..), Thread, gsvCode, bcoCode)
import GSI.Eval (evalSync)

apiCall :: GSEvalState -> Pos -> GSValue -> Thread -> IO GSValue
apiCall evs pos0 (GSImplementationFailure pos1 e) t = throwIO $ GSExcImplementationFailure pos1 e
apiCall evs pos (GSInvalidProgram err) t = throwIO $ GSExcInvalidProgram err
apiCall evs pos (GSError err) t = throwIO $ GSExcError err
apiCall evs pos (GSThunk th) t = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace pos []] th
    apiCall evs pos v t
apiCall evs pos0 (GSClosure cs bco) t = case bco of
    GSImp a -> a evs t
    _ -> throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is (GSClosure cs " ++ bcoCode bco ++ ")) next"
apiCall evs pos v t = do
    throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallExpr :: GSEvalState -> Pos -> GSExpr -> Thread -> IO GSValue
apiCallExpr evs pos (GSExpr e) t = do
    v <- e evs [StackTrace pos []] GSExprCont{ gsreturn = return, gsthrow = return }
    apiCall evs pos v t

apiImplementationFailure = varE 'apiImplementationFailure_w `appE` gshere

apiImplementationFailure_w pos err = throwIO $ GSExcImplementationFailure pos err
