{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (aceEnter, aceForce, aceArg, aceField) where

import qualified Data.Map as Map

import GSI.Util (Pos, StackTrace(..), fmtPos)
import GSI.Syn (GSVar, fmtVarAtom)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr(..), GSExprCont(..), GSThunkState(..), gsimplementationfailure, gsvCode, bcoCode)
import {-# SOURCE #-} GSI.Eval (GSResult(..), evalSync)

aceEnter :: [StackTrace] -> GSValue -> GSExprCont a -> IO a
aceEnter cs v@GSError{} sk = gsthrow sk v
aceEnter cs v@GSImplementationFailure{} sk = gsthrow sk v
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
    _ -> gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = GSCLosure " ++ bcoCode bco ++") next"
aceEnter cs v@GSExternal{} sk = gsreturn sk v
aceEnter cs e sk = gsthrow sk $ $gsimplementationfailure $ "aceEnter (expr = " ++ gsvCode e ++") next"

aceForce :: StackTrace -> (GSValue -> GSExpr) -> GSExprCont a -> GSExprCont a
aceForce c k sk = GSExprCont {
    gsreturn = \ v -> runGSExpr (k v) [c] sk,
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
        f -> gsthrow sk $ $gsimplementationfailure $ "aceArg (function is " ++ gsvCode f ++ " next"
      ,
    gsthrow = gsthrow sk
  }

aceField :: StackTrace -> GSVar -> GSExprCont a -> GSExprCont a
aceField c1 f sk = GSExprCont{
    gsreturn = \ r -> case r of
        GSRecord pos1 fs -> case Map.lookup f fs of
            Just v -> aceEnter [c1] v sk
            Nothing -> gsthrow sk $ $gsimplementationfailure $ (fmtPos pos1 . ("missing field "++) . fmtVarAtom f) $ ""
        _ -> gsthrow sk $ $gsimplementationfailure $ "aceField " ++ gsvCode r ++ " next"
      ,
    gsthrow = gsthrow sk
  }
