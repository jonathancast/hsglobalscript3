{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterThunkState, aceEnterIntExpr, aceUpdate, aceForce, aceArg, aceField, aceReHere, aceEmptyStack) where

import qualified Data.Map as Map

import Control.Monad (join)

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (Pos(..), StackTrace(..), gshere, fmtPos, fmtCallers)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..), GSInvalidProgram(..), errCode)
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (newEvent, wakeup, await, OPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSIntExpr(..), GSExprCont(..), GSThunkState(..), GSExternal(..), gsintprepare, gsexternal, gsrehere_w, gsimplementationfailure, whichExternal, gsvCode, bcoCode, gstsCode, iexprCode)

aceEnter :: OPort Message -> Maybe ProfCounter -> [StackTrace] -> GSValue -> GSExprCont a -> IO a
aceEnter msg pc cs v@GSInvalidProgram{} sk = gsthrow sk v
aceEnter msg pc cs v@GSError{} sk = gsthrow sk v
aceEnter msg pc cs v@GSImplementationFailure{} sk = gsthrow sk v
aceEnter msg pc cs (GSThunk mv) sk = join $ modifyMVar mv $ \ st -> case st of
    GSTSExpr{} -> doEval st
    GSTSIntExpr{} -> doEval st
    GSApply{} -> doEval st
    GSTSField{} -> doEval st
    GSTSIndirection v -> return (GSTSIndirection v, aceEnter msg pc cs v sk)
    GSTSStack e -> return (GSTSStack e, await e >> aceEnter msg pc cs (GSThunk mv) sk)
    _ -> return (st, gsthrow sk $ $gsimplementationfailure $ "aceEnter (thunk: " ++ gstsCode st ++ ") next")
  where
    doEval st = do
        e <- newEvent
        return (GSTSStack e, aceEnterThunkState msg pc cs st (aceUpdate mv sk))
aceEnter msg pc cs v@GSConstr{} sk = gsreturn sk v
aceEnter msg pc cs v@GSRecord{} sk = gsreturn sk v
aceEnter msg pc cs v@GSRune{} sk = gsreturn sk v
aceEnter msg pc cs v@GSNatural{} sk = gsreturn sk v
aceEnter msg pc cs v@GSRational{} sk = gsreturn sk v
aceEnter msg pc cs0 v@(GSClosure cs1 bco) sk = case bco of
    GSRawExpr e -> runGSExpr e msg pc (cs1 ++ cs0) sk
    GSImp{} -> gsreturn sk v
    GSLambda{} -> gsreturn sk v
    _ -> gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next"
aceEnter msg pc cs v@GSExternal{} sk = gsreturn sk v
aceEnter msg pc cs e sk = gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterThunkState :: OPort Message -> Maybe ProfCounter -> [StackTrace] -> GSThunkState -> GSExprCont a -> IO a
aceEnterThunkState msg pc cs (GSTSExpr expr) sk = expr msg pc cs sk
aceEnterThunkState msg pc cs (GSTSIntExpr e) sk = aceEnterIntExpr msg pc cs e sk
aceEnterThunkState msg pc cs (GSApply pos fn args) sk =
    aceEnter msg pc (StackTrace pos [] : cs) fn (aceArg msg pc (StackTrace pos []) args sk)
aceEnterThunkState msg pc cs (GSTSField pos f r) sk =
    aceEnter msg pc (StackTrace pos [] : cs) r (aceField msg pc (StackTrace pos []) f sk)
aceEnterThunkState msg pc cs st sk = gsthrow sk $ $gsimplementationfailure $ "aceEnterThunkState (thunk: " ++ gstsCode st ++ ") next"

aceEnterIntExpr :: OPort Message -> Maybe ProfCounter -> [StackTrace] -> GSIntExpr -> GSExprCont a -> IO a
aceEnterIntExpr msg pc cs (GSIntWithHere pos k) sk =
    aceEnterIntExpr msg pc [StackTrace pos cs] k $ aceArg msg pc (StackTrace pos cs) [gsexternal $ StackTrace pos cs] $ sk
aceEnterIntExpr msg pc cs (GSIntUndefined pos) sk = gsthrow sk $ GSError $ GSErrUnimpl $ StackTrace pos cs
aceEnterIntExpr msg pc cs (GSIntGEnter pos v) sk = aceEnter msg pc [StackTrace pos cs] v sk
aceEnterIntExpr msg pc cs (GSIntBEnter pos e) sk = runGSExpr e msg pc [StackTrace pos cs] sk
aceEnterIntExpr msg pc cs (GSIntEApply pos f as) sk = do
    avs <- mapM gsintprepare as
    aceEnterIntExpr msg pc [StackTrace pos cs] f (aceArg msg pc (StackTrace pos cs) avs sk)
aceEnterIntExpr msg pc cs (GSIntGApply pos f as) sk = do
    avs <- mapM gsintprepare as
    aceEnter msg pc [StackTrace pos cs] f (aceArg msg pc (StackTrace pos cs) avs sk)
aceEnterIntExpr msg pc cs e sk = gsthrow sk $ $gsimplementationfailure $ "aceEnterIntExpr " ++ iexprCode e ++ " next"

aceUpdate :: MVar (GSThunkState) -> GSExprCont a -> GSExprCont a
aceUpdate mv sk = GSExprCont{
    gsreturn = \ v -> updateThunk v >> gsreturn sk v,
    gsthrow = \ v -> updateThunk v >> gsthrow sk v
  }
  where
    updateThunk v = do
        mbb <- modifyMVar mv $ \ s -> case s of
            GSTSStack b -> return (GSTSIndirection v, Just b)
            _ -> return (GSTSIndirection v, Nothing)
        maybe (return ()) wakeup mbb

aceForce :: OPort Message -> Maybe ProfCounter -> [StackTrace] -> (GSValue -> GSExpr) -> GSExprCont a -> GSExprCont a
aceForce msg pc cs k sk = GSExprCont {
    gsreturn = \ v -> runGSExpr (k v) msg pc cs sk,
    gsthrow = gsthrow sk
  }

aceArg :: OPort Message -> Maybe ProfCounter -> StackTrace -> [GSValue] -> GSExprCont a -> GSExprCont a
aceArg msg pc c1 [] sk = sk
aceArg msg pc c1 as sk = GSExprCont {
    gsreturn = \ v -> gsapplyFunction msg pc c1 v as sk,
    gsthrow = gsthrow sk
  }

gsapplyFunction :: OPort Message -> Maybe ProfCounter -> StackTrace -> GSValue -> [GSValue] -> GSExprCont a -> IO a
gsapplyFunction msg pc c1 v [] sk = gsreturn sk v
gsapplyFunction msg pc c1 (GSClosure cs (GSLambda f)) (a:as) sk = case f a of
    GSRawExpr e -> runGSExpr e msg pc (cs ++ [c1]) (aceArg msg pc c1 as sk)
    bco@GSImp{} -> gsapplyFunction  msg pc c1 (GSClosure (cs ++ [c1]) bco) as sk
    bco@GSLambda{} -> gsapplyFunction msg pc c1 (GSClosure cs bco) as sk
    bco -> gsthrow sk $ $gsimplementationfailure $ "gsapplyFunction (result is " ++ bcoCode bco ++ ") next"
gsapplyFunction msg pc c1 (GSClosure cs (GSImp _)) as sk = gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError c1 "gsapplyFunction" ("GSImp " ++ fmtCallers cs "") "function"
gsapplyFunction msg pc c1 (GSClosure cs bco) as sk = gsthrow sk $ $gsimplementationfailure $ "gsapplyFunction (GSClosure cs " ++ bcoCode bco ++ ") next"
gsapplyFunction msg pc c1 (GSConstr pos _ _) as sk = gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError c1 "gsapplyFunction" ("GSConstr " ++ fmtPos pos "") "closure"
gsapplyFunction msg pc c1 (GSRecord pos _) as sk = gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError c1 "gsapplyFunction" ("GSRecord " ++ fmtPos pos "") "closure"
gsapplyFunction msg pc c1 f as sk = gsthrow sk $ $gsimplementationfailure $ "gsapplyFunction " ++ gsvCode f ++ ") next"

aceField :: OPort Message -> Maybe ProfCounter -> StackTrace -> GSVar -> GSExprCont a -> GSExprCont a
aceField msg pc c1 f sk = GSExprCont{
    gsreturn = \ r -> case r of
        GSRecord pos1 fs -> case Map.lookup f fs of
            Just v -> aceEnter msg pc [c1] v sk
            Nothing -> gsthrow sk $ $gsimplementationfailure $ (fmtPos pos1 . ("missing field "++) . fmtVarAtom f) $ ""
        GSExternal e | Just (Pos fn l c) <- fromExternal e -> case f of
            _ | f == gsvar "filename" -> gsreturn sk $ foldr (\ ch sv -> GSConstr $gshere (gsvar ":") [ GSRune ch, sv ]) (GSConstr $gshere (gsvar "nil") []) fn
            _ | f == gsvar "line" -> gsreturn sk $ GSNatural l
            _ | f == gsvar "col" -> gsreturn sk $ GSNatural c
            _ -> gsthrow sk $ $gsimplementationfailure $ "aceField pos " ++ fmtVarAtom f " next"
        GSExternal e -> gsthrow sk $ $gsimplementationfailure $ "aceField " ++ whichExternal e ++ " next"
        GSClosure cs bco -> gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError c1 ("aceField" ++ ' ' : fmtVarAtom f "") ("GSClosure at " ++ fmtCallers cs "") "record"
        GSConstr pos c fs -> gsthrow sk $ GSInvalidProgram $ GSIPRuntimeTypeError c1 ("aceField" ++ ' ' : fmtVarAtom f "") (("GSConstr "++) $ fmtVarAtom c $ (" at "++) $ fmtPos pos $ "") "record"
        _ -> gsthrow sk $ $gsimplementationfailure $ "aceField " ++ gsvCode r ++ " next"
      ,
    gsthrow = gsthrow sk
  }

aceReHere :: Pos -> [StackTrace] -> GSExprCont a -> GSExprCont a
aceReHere pos cs sk = GSExprCont{
    gsreturn = \ r -> gsreturn sk $ gsrehere_w pos cs r,
    gsthrow = \ e -> gsthrow sk $ gsrehere_w pos cs e
  }

aceEmptyStack :: GSExprCont ()
aceEmptyStack = GSExprCont{ gsreturn = const $ return (), gsthrow = const $ return () }
