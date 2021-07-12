{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (GSResult(..), eval, evalSync, stCode) where

import Control.Concurrent (MVar, forkIO, modifyMVar)

import GSI.Util (StackTrace(..))
import GSI.RTS (Event, newEvent, await)
import GSI.Value (GSValue(..), GSBCO(..), GSEvalState(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gstsCode)

import ACE (aceEnterThunkState, aceUpdate, aceEmptyStack)

data GSResult
  = GSStack Event
  | GSIndirection GSValue
  | GSWHNF

eval :: GSEvalState -> [StackTrace] -> MVar (GSThunkState) -> IO GSResult
eval evs cs mv = modifyMVar mv $ \ st -> case st of
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
        forkIO $ aceEnterThunkState evs cs st (aceUpdate mv aceEmptyStack)
        return (GSTSStack e, GSStack e)

evalSync :: GSEvalState -> [StackTrace] -> MVar (GSThunkState) -> IO GSValue
evalSync evs cs mv = do
    st <- eval evs cs mv
    case st of
        GSStack b -> await b *> evalSync evs cs mv
        GSIndirection v -> case v of
            GSImplementationFailure{} -> return v
            GSInvalidProgram{} -> return v
            GSError{} -> return v
            GSThunk th -> evalSync evs cs th
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
