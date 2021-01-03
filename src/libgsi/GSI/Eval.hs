{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (GSResult(..), eval, evalSync, stCode) where

import Control.Concurrent (MVar, forkIO, modifyMVar)

import GSI.Util (StackTrace(..), gshere)
import GSI.RTS (Event, newEvent, await, OPort)
import GSI.Error (GSError(..))
import GSI.Message (Message)
import GSI.Value (GSValue(..), GSBCO(..), GSExprCont(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gstsCode)

import ACE (aceEnter, aceEnterThunkState, aceUpdate, aceArg, aceField, aceEmptyStack)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

eval :: OPort Message -> [StackTrace] -> MVar (GSThunkState) -> IO GSResult
eval msg cs mv = modifyMVar mv $ \ st -> case st of
    GSTSExpr{} -> startEval st
    GSTSIntExpr{} -> startEval st
    GSApply{} -> startEval st
    GSTSField{} -> startEval st
    GSTSIndirection v -> return (GSTSIndirection v, GSIndirection v)
    GSTSStack e -> return (GSTSStack e, GSStack e)
    _ -> return (st, GSIndirection $ $gsimplementationfailure $ "eval (thunk: " ++ gstsCode st ++ ") next")
  where
    startEval st = do
        e <- newEvent
        forkIO $ aceEnterThunkState msg cs st (aceUpdate mv aceEmptyStack)
        return (GSTSStack e, GSStack e)

evalSync :: OPort Message -> [StackTrace] -> MVar (GSThunkState) -> IO GSValue
evalSync msg cs mv = do
    st <- eval msg cs mv
    case st of
        GSStack b -> await b *> evalSync msg cs mv
        GSIndirection v -> case v of
            GSImplementationFailure{} -> return v
            GSInvalidProgram{} -> return v
            GSError{} -> return v
            GSThunk th -> evalSync msg cs th
            GSConstr{} -> return v
            GSRecord{} -> return v
            GSNatural{} -> return v
            GSRune{} -> return v
            v@(GSClosure _ bco) -> case bco of
                GSImp{} -> return v
                GSLambda{} -> return v
                _ -> return $ $gsimplementationfailure $ "evalSync (GSIndirection (GSClosure _ " ++ bcoCode bco ++ ")) next"
            GSExternal{} -> return v
            _ -> return $ $gsimplementationfailure $ "evalSync (GSIndirection " ++ gsvCode v ++ ") next"
        _ -> return $ $gsimplementationfailure $ "evalSync " ++ stCode st ++ " next"

stCode :: GSResult -> String
stCode GSStack{} = "GSStack"
stCode GSIndirection{} = "GSIndirection"
stCode GSWHNF{} = "GSWHNF"
