{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceForce, aceArg, aceThrow) where

import GSI.Util (Pos, StackTrace(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> GSExprCont a -> IO a
aceEnter cs v@GSError{} sk = aceThrow v sk
aceEnter cs v@GSImplementationFailure{} sk = aceThrow v sk
aceEnter cs (GSThunk th) sk = do
    v <- evalSync cs th
    aceEnter cs v sk
aceEnter cs v@GSConstr{} sk = gsreturn sk v
aceEnter cs v@GSRecord{} sk = gsreturn sk v
aceEnter cs v@GSRune{} sk = gsreturn sk v
aceEnter cs v@GSNatural{} sk = gsreturn sk v
aceEnter cs0 v@(GSClosure cs1 bco) sk = case bco of
    GSRawExpr e -> runGSExpr e (cs1 ++ cs0) sk
    GSImp{} -> gsreturn sk v
    GSLambda{} -> gsreturn sk v
    _ -> aceThrow ($gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next") sk
aceEnter cs v@GSExternal{} sk = gsreturn sk v
aceEnter cs e sk = aceThrow ($gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next") sk

aceForce :: StackTrace -> (GSValue -> GSExpr) -> GSExprCont a -> GSExprCont a
aceForce c k sk = GSExprCont {
    gsreturn = \ v -> runGSExpr (k v) [c] sk,
    gsthrow = \ v -> aceThrow v sk
  }

aceArg :: StackTrace -> GSValue -> GSExprCont a -> GSExprCont a
aceArg c1 a sk = GSExprCont {
    gsreturn = \ v -> case v of
        GSClosure cs (GSLambda f) -> case f a of
            GSRawExpr e -> runGSExpr e (cs ++ [c1]) sk
            bco@GSImp{} -> gsreturn sk $ GSClosure (cs ++ [c1]) bco
            bco@GSLambda{} -> gsreturn sk $ GSClosure (cs ++ [c1]) bco
            bco -> aceThrow ($gsimplementationfailure $ "aceArg (function; result is " ++ bcoCode bco ++ ") next") sk
        GSClosure cs bco -> aceThrow ($gsimplementationfailure $ "aceArg (function is (GSClosure cs " ++ bcoCode bco ++ ")) next") sk
        f -> aceThrow ($gsimplementationfailure $ "aceArg (function is " ++ gsvCode f ++ " next") sk
      ,
    gsthrow = \ v -> aceThrow v sk
  }

aceThrow :: GSValue -> GSExprCont a -> IO a
aceThrow v sk = gsthrow sk v
