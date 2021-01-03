{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module API (apiCall, apiCallExpr, apiImplementationFailure, apiImplementationFailure_w) where

import Control.Exception (throwIO)

import Language.Haskell.TH.Lib (appE, varE)

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Error (GSException(..))
import GSI.Message (Message)
import GSI.RTS (OPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), gsvCode, bcoCode)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread)

apiCall :: OPort Message -> Pos -> GSValue -> Thread -> IO GSValue
apiCall msg pos0 (GSImplementationFailure pos1 e) t = throwIO $ GSExcImplementationFailure pos1 e
apiCall msg pos (GSInvalidProgram err) t = throwIO $ GSExcInvalidProgram err
apiCall msg pos (GSError err) t = throwIO $ GSExcError err
apiCall msg pos (GSThunk th) t = do
    v <- evalSync msg [StackTrace pos []] th
    apiCall msg pos v t
apiCall msg pos0 (GSClosure cs bco) t = case bco of
    GSImp a -> a msg t
    _ -> throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is (GSClosure cs " ++ bcoCode bco ++ ")) next"
apiCall msg pos v t = do
    throwIO $ GSExcImplementationFailure $gshere $ "runThread (state is ThreadStateRunning; code is non-empty; next statement is " ++ gsvCode v ++ ") next"

apiCallExpr :: OPort Message -> Pos -> GSExpr -> Thread -> IO GSValue
apiCallExpr msg pos (GSExpr e) t = do
    v <- e msg [StackTrace pos []] GSExprCont{ gsreturn = return, gsthrow = return }
    apiCall msg pos v t

apiImplementationFailure = varE 'apiImplementationFailure_w `appE` gshere

apiImplementationFailure_w pos err = throwIO $ GSExcImplementationFailure pos err
