{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterThunkState, aceEnterIntExpr, aceUpdate, aceForce, aceArg, aceField, aceEmptyStack) where

import qualified Data.Map as Map

import Control.Monad (join)

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (Pos(..), StackTrace(..), gshere, fmtPos)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..))
import GSI.RTS (newEvent, wakeup, await)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSIntExpr(..), GSExprCont(..), GSThunkState(..), GSExternal(..), gsintprepare, gsexternal, gsimplementationfailure, whichExternal, gsvCode, bcoCode, gstsCode, iexprCode)

aceEnter :: [StackTrace] -> GSValue -> GSExprCont a -> IO a
aceEnter cs v@GSInvalidProgram{} sk = gsthrow sk v
aceEnter cs v@GSError{} sk = gsthrow sk v
aceEnter cs v@GSImplementationFailure{} sk = gsthrow sk v
aceEnter cs (GSThunk mv) sk = join $ modifyMVar mv $ \ st -> case st of
    GSTSExpr{} -> doEval st
    GSTSIntExpr{} -> doEval st
    GSApply{} -> doEval st
    GSTSField{} -> doEval st
    GSTSIndirection v -> return (GSTSIndirection v, aceEnter cs v sk)
    GSTSStack e -> return (GSTSStack e, await e >> aceEnter cs (GSThunk mv) sk)
    _ -> return (st, gsthrow sk $ $gsimplementationfailure $ "aceEnter (thunk: " ++ gstsCode st ++ ") next")
  where
    doEval st = do
        e <- newEvent
        return (GSTSStack e, aceEnterThunkState cs st (aceUpdate mv sk))
aceEnter cs v@GSConstr{} sk = gsreturn sk v
aceEnter cs v@GSRecord{} sk = gsreturn sk v
aceEnter cs v@GSRune{} sk = gsreturn sk v
aceEnter cs v@GSNatural{} sk = gsreturn sk v
aceEnter cs0 v@(GSClosure cs1 bco) sk = case bco of
    GSRawExpr e -> runGSExpr e (cs1 ++ cs0) sk
    GSImp{} -> gsreturn sk v
    GSLambda{} -> gsreturn sk v
    _ -> gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next"
aceEnter cs v@GSExternal{} sk = gsreturn sk v
aceEnter cs e sk = gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceEnterThunkState :: [StackTrace] -> GSThunkState -> GSExprCont a -> IO a
aceEnterThunkState cs (GSTSExpr expr) sk = expr cs sk
aceEnterThunkState cs (GSTSIntExpr e) sk = aceEnterIntExpr cs e sk
aceEnterThunkState cs (GSApply pos fn args) sk =
    aceEnter (StackTrace pos [] : cs) fn (foldr (aceArg (StackTrace pos [])) sk args)
aceEnterThunkState cs (GSTSField pos f r) sk =
    aceEnter (StackTrace pos [] : cs) r (aceField (StackTrace pos []) f sk)
aceEnterThunkState cs st sk = gsthrow sk $ $gsimplementationfailure $ "aceEnterThunkState (thunk: " ++ gstsCode st ++ ") next"

aceEnterIntExpr :: [StackTrace] -> GSIntExpr -> GSExprCont a -> IO a
aceEnterIntExpr cs (GSIntWithHere pos k) sk =
    aceEnterIntExpr [StackTrace pos cs] k $ aceArg (StackTrace pos cs) (gsexternal $ StackTrace pos cs) $ sk
aceEnterIntExpr cs (GSIntUndefined pos) sk = gsthrow sk $ GSError $ GSErrUnimpl $ StackTrace pos cs
aceEnterIntExpr cs (GSIntGEnter pos v) sk = aceEnter [StackTrace pos cs] v sk
aceEnterIntExpr cs (GSIntBEnter pos e) sk = runGSExpr e [StackTrace pos cs] sk
aceEnterIntExpr cs (GSIntEApply pos f as) sk = do
    avs <- mapM gsintprepare as
    aceEnterIntExpr [StackTrace pos cs] f (foldr (aceArg (StackTrace pos cs)) sk avs)
aceEnterIntExpr cs (GSIntGApply pos f as) sk = do
    avs <- mapM gsintprepare as
    aceEnter [StackTrace pos cs] f (foldr (aceArg (StackTrace pos cs)) sk avs)
aceEnterIntExpr cs e sk = gsthrow sk $ $gsimplementationfailure $ "aceEnterIntExpr " ++ iexprCode e ++ " next"

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

aceForce :: [StackTrace] -> (GSValue -> GSExpr) -> GSExprCont a -> GSExprCont a
aceForce cs k sk = GSExprCont {
    gsreturn = \ v -> runGSExpr (k v) cs sk,
    gsthrow = gsthrow sk
  }

aceArg :: StackTrace -> GSValue -> GSExprCont a -> GSExprCont a
aceArg c1 a sk = GSExprCont {
    gsreturn = \ v -> case v of
        GSClosure cs (GSLambda f) -> case f a of
            GSRawExpr e -> runGSExpr e (cs ++ [c1]) sk
            bco@GSImp{} -> gsreturn sk $ GSClosure (cs ++ [c1]) bco
            bco@GSLambda{} -> gsreturn sk $ GSClosure (cs ++ [c1]) bco
            bco -> gsthrow sk $ $gsimplementationfailure $ "aceArg (function; result is " ++ bcoCode bco ++ ") next"
        GSClosure cs bco -> gsthrow sk $ $gsimplementationfailure $ "aceArg (function is (GSClosure cs " ++ bcoCode bco ++ ")) next"
        f -> gsthrow sk $ $gsimplementationfailure $ "aceArg (function is " ++ gsvCode f ++ ") next"
      ,
    gsthrow = gsthrow sk
  }

aceField :: StackTrace -> GSVar -> GSExprCont a -> GSExprCont a
aceField c1 f sk = GSExprCont{
    gsreturn = \ r -> case r of
        GSRecord pos1 fs -> case Map.lookup f fs of
            Just v -> aceEnter [c1] v sk
            Nothing -> gsthrow sk $ $gsimplementationfailure $ (fmtPos pos1 . ("missing field "++) . fmtVarAtom f) $ ""
        GSExternal e | Just (Pos fn l c) <- fromExternal e -> case f of
            _ | f == gsvar "filename" -> gsreturn sk $ foldr (\ ch sv -> GSConstr $gshere (gsvar ":") [ GSRune ch, sv ]) (GSConstr $gshere (gsvar "nil") []) fn
            _ | f == gsvar "line" -> gsreturn sk $ GSNatural l
            _ | f == gsvar "col" -> gsreturn sk $ GSNatural c
            _ -> gsthrow sk $ $gsimplementationfailure $ "aceField pos " ++ fmtVarAtom f " next"
        GSExternal e -> gsthrow sk $ $gsimplementationfailure $ "aceField " ++ whichExternal e ++ " next"
        _ -> gsthrow sk $ $gsimplementationfailure $ "aceField " ++ gsvCode r ++ " next"
      ,
    gsthrow = gsthrow sk
  }

aceEmptyStack :: GSExprCont ()
aceEmptyStack = GSExprCont{ gsreturn = const $ return (), gsthrow = const $ return () }
