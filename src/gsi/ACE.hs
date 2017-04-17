{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceEnterExpr, aceReturn, aceThrow) where

import GSI.Util (Pos, StackTrace(..))
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSStackFrame(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode, gsstCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> [GSStackFrame] -> IO GSValue
aceEnter cs v@GSError{} st = aceThrow v st
aceEnter cs v@GSImplementationFailure{} st = aceThrow v st
aceEnter cs (GSThunk th) st = do
    v <- evalSync cs th
    aceEnter cs v st
aceEnter cs v@GSConstr{} st = aceReturn v st
aceEnter cs v@GSRecord{} st = aceReturn v st
aceEnter cs v@GSRune{} st = aceReturn v st
aceEnter cs v@GSNatural{} st = aceReturn v st
aceEnter cs0 v@(GSClosure cs1 bco) st = case bco of
    GSRawExpr e -> aceEnterExpr (cs1 ++ cs0) e st
    GSImp{} -> aceReturn v st
    GSLambda{} -> aceReturn v st
    _ -> aceThrow ($gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next") st
aceEnter cs e st = aceThrow ($gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next") st

aceEnterExpr :: [StackTrace] -> GSExpr -> [GSStackFrame] -> IO GSValue
aceEnterExpr cs (GSExpr e) st = e st cs

aceReturn :: GSValue -> [GSStackFrame] -> IO GSValue
aceReturn (GSClosure cs (GSLambda f)) (k@(GSStackArg c1 a):st) = case f a of
    GSRawExpr e -> aceEnterExpr (cs ++ [c1]) e st
    bco@GSImp{} -> aceReturn (GSClosure (cs ++ [c1]) bco) st
    bco@GSLambda{} -> aceReturn (GSClosure (cs ++ [c1]) bco) st
    bco -> aceThrow ($gsimplementationfailure $ "aceReturn (function; result is " ++ bcoCode bco ++ ") next") st
aceReturn (GSClosure cs bco) (k@(GSStackArg pos a):st) = aceThrow ($gsimplementationfailure $ "aceReturn (function is (GSClosure cs " ++ bcoCode bco ++ "); continuation is argument) next") (k:st)
aceReturn f (k@(GSStackArg pos a):st) = aceThrow ($gsimplementationfailure $ "aceReturn (function is " ++ gsvCode f ++ "; continuation is argument) next") (k:st)
aceReturn v (GSStackForce c k:st) = aceEnterExpr [c] (k v) st
aceReturn v (k:st) = aceThrow ($gsimplementationfailure $ "aceReturn (continuation is " ++ gsstCode k ++ ") next") (k:st)
aceReturn v [] = return v

aceThrow :: GSValue -> [GSStackFrame] -> IO GSValue
aceThrow v (GSStackForce{}:st) = aceThrow v st
aceThrow v (GSStackArg{}:st) = aceThrow v st
aceThrow v (k:st) = return $ $gsimplementationfailure $ "aceThrow (continuation is " ++ gsstCode k ++ ") next"
aceThrow v [] = return v
