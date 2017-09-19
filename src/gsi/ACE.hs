{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterExpr, aceReturn, aceThrow) where

import GSI.Util (Pos, StackTrace(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> [GSStackFrame] -> GSExprCont a -> IO a
aceEnter cs v@GSError{} st sk = aceThrow v st sk
aceEnter cs v@GSImplementationFailure{} st sk = aceThrow v st sk
aceEnter cs (GSThunk th) st sk = do
    v <- evalSync cs th
    aceEnter cs v st sk
aceEnter cs v@GSConstr{} st sk = aceReturn v st sk
aceEnter cs v@GSRecord{} st sk = aceReturn v st sk
aceEnter cs v@GSRune{} st sk = aceReturn v st sk
aceEnter cs v@GSNatural{} st sk = aceReturn v st sk
aceEnter cs0 v@(GSClosure cs1 bco) st sk = case bco of
    GSRawExpr e -> aceEnterExpr (cs1 ++ cs0) e st sk
    GSImp{} -> aceReturn v st sk
    GSLambda{} -> aceReturn v st sk
    _ -> aceThrow ($gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next") st sk
aceEnter cs v@GSExternal{} st sk = aceReturn v st sk
aceEnter cs e st sk = aceThrow ($gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next") st sk

aceEnterExpr :: [StackTrace] -> GSExpr -> [GSStackFrame] -> GSExprCont a -> IO a
aceEnterExpr cs (GSExpr e) st k = e st cs k

aceReturn :: GSValue -> [GSStackFrame] -> GSExprCont a -> IO a
aceReturn (GSClosure cs (GSLambda f)) (k@(GSStackArg c1 a):st) sk = case f a of
    GSRawExpr e -> aceEnterExpr (cs ++ [c1]) e st sk
    bco@GSImp{} -> aceReturn (GSClosure (cs ++ [c1]) bco) st sk
    bco@GSLambda{} -> aceReturn (GSClosure (cs ++ [c1]) bco) st sk
    bco -> aceThrow ($gsimplementationfailure $ "aceReturn (function; result is " ++ bcoCode bco ++ ") next") st sk
aceReturn (GSClosure cs bco) (k@(GSStackArg pos a):st) sk = aceThrow ($gsimplementationfailure $ "aceReturn (function is (GSClosure cs " ++ bcoCode bco ++ "); continuation is argument) next") (k:st) sk
aceReturn f (k@(GSStackArg pos a):st) sk = aceThrow ($gsimplementationfailure $ "aceReturn (function is " ++ gsvCode f ++ "; continuation is argument) next") (k:st) sk
aceReturn v (GSStackForce c k:st) sk = aceEnterExpr [c] (k v) st sk
aceReturn v (k:st) sk = aceThrow ($gsimplementationfailure $ "aceReturn (continuation is " ++ gsstCode k ++ ") next") (k:st) sk
aceReturn v [] sk = gsreturn sk v

aceThrow :: GSValue -> [GSStackFrame] -> GSExprCont a -> IO a
aceThrow v (GSStackForce{}:st) sk = aceThrow v st sk
aceThrow v (GSStackArg{}:st) sk = aceThrow v st sk
aceThrow v (k:st) sk = gsthrow sk $ $gsimplementationfailure $ "aceThrow (continuation is " ++ gsstCode k ++ ") next"
aceThrow v [] sk = gsthrow sk v
