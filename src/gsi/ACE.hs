{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterExpr, aceForce, aceArg, aceReturn, aceThrow) where

import GSI.Util (Pos, StackTrace(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> GSExprCont a -> IO a
aceEnter cs v@GSError{} sk = aceThrow v sk
aceEnter cs v@GSImplementationFailure{} sk = aceThrow v sk
aceEnter cs (GSThunk th) sk = do
    v <- evalSync cs th
    aceEnter cs v sk
aceEnter cs v@GSConstr{} sk = aceReturn v sk
aceEnter cs v@GSRecord{} sk = aceReturn v sk
aceEnter cs v@GSRune{} sk = aceReturn v sk
aceEnter cs v@GSNatural{} sk = aceReturn v sk
aceEnter cs0 v@(GSClosure cs1 bco) sk = case bco of
    GSRawExpr e -> aceEnterExpr (cs1 ++ cs0) e sk
    GSImp{} -> aceReturn v sk
    GSLambda{} -> aceReturn v sk
    _ -> aceThrow ($gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next") sk
aceEnter cs v@GSExternal{} sk = aceReturn v sk
aceEnter cs e sk = aceThrow ($gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next") sk

aceEnterExpr :: [StackTrace] -> GSExpr -> GSExprCont a -> IO a
aceEnterExpr cs (GSExpr e) k = e cs k

aceReturn :: GSValue -> GSExprCont a -> IO a
aceReturn v sk = gsreturn sk v

aceForce :: StackTrace -> (GSValue -> GSExpr) -> GSExprCont a -> GSExprCont a
aceForce c k sk = GSExprCont {
    gsreturn = \ v -> aceEnterExpr [c] (k v) sk,
    gsthrow = \ v -> aceThrow v sk
  }

aceArg :: StackTrace -> GSValue -> GSExprCont a -> GSExprCont a
aceArg c1 a sk = GSExprCont {
    gsreturn = \ v -> case v of
        GSClosure cs (GSLambda f) -> case f a of
            GSRawExpr e -> aceEnterExpr (cs ++ [c1]) e sk
            bco@GSImp{} -> aceReturn (GSClosure (cs ++ [c1]) bco) sk
            bco@GSLambda{} -> aceReturn (GSClosure (cs ++ [c1]) bco) sk
            bco -> aceThrow ($gsimplementationfailure $ "aceReturn (function; result is " ++ bcoCode bco ++ ") next") sk
        GSClosure cs bco -> aceThrow ($gsimplementationfailure $ "aceReturn (function is (GSClosure cs " ++ bcoCode bco ++ "); continuation is argument) next") sk
        f -> aceThrow ($gsimplementationfailure $ "aceReturn (function is " ++ gsvCode f ++ "; continuation is argument) next") sk
      ,
    gsthrow = \ v -> aceThrow v sk
  }

aceThrow :: GSValue -> GSExprCont a -> IO a
aceThrow v sk = gsthrow sk v
