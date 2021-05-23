{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallExpr, apiImplementationFailure, apiImplementationFailure_w) where

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Error (GSException(..))
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (OPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSEvalState(..), GSExprCont(..), gsvCode, bcoCode)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread)

apiCall :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> Thread -> IO GSValue
apiCall msg pc pos0 (GSImplementationFailure pos1 e) t = throwIO $ GSExcImplementationFailure pos1 e
apiCall msg pc pos (GSInvalidProgram err) t = throwIO $ GSExcInvalidProgram err
apiCall msg pc pos (GSError err) t = throwIO $ GSExcError err
apiCall msg pc pos (GSThunk th) t = do
    v <- evalSync msg pc [StackTrace pos []] th
    apiCall msg pc pos v t
apiCall msg pc pos0 (GSClosure cs bco) t = case bco of
    GSImp a -> a (GSEvalState msg pc) t
    _ -> throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is (GSClosure cs " ++ bcoCode bco ++ ")) next"
apiCall msg pc pos v t = do
    throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallExpr :: OPort Message -> Maybe ProfCounter -> Pos -> GSExpr -> Thread -> IO GSValue
apiCallExpr msg pc pos (GSExpr e) t = do
    v <- e (GSEvalState msg pc) [StackTrace pos []] GSExprCont{ gsreturn = return, gsthrow = return }
    apiCall msg pc pos v t

apiImplementationFailure = varE 'apiImplementationFailure_w `appE` gshere

apiImplementationFailure_w pos err = throwIO $ GSExcImplementationFailure pos err
