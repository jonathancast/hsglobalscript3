{-# LANGUAGE TemplateHaskell, ExistentialQuantification, ScopedTypeVariables #-}
module GSI.GSI (
    gsi_monad,
    gsigsinject, gsigsthunk, gsigsintthunk, gsigsapply, gsigsundefined, gsigsav, gsigsae,
    gsigsbcarg, gsigsbcrehere, gsigsbclfield, gsigsbcapply, gsigsbcundefined, gsigsbcnatural, gsigsbcenter, gsigsbcinsufficientcases, gsigsbcdiscardpattern, gsigsbcvarpattern, gsigsbcviewpattern,
    gsigsintbcwithhere, gsigsintbcopenexpr, gsigsintbcgenter, gsigsintbcfenter, gsigsintbcgapply, gsigsintbceapply, gsigsintbcnatural, gsigsintbcundefined, gsigsintbcinsufficientcases,
    gsigsintbcvarpattern, gsigsintbcdiscardpattern,
    gsigsiae, gsigsiagv,
    gsigsvar, gsigsvar_view,
    gsieval_sync, gsigsfmtError,
    gsicreateThread, gsiexecMainThread,
    gsigsvar_eq, gsigsvar_compare, gsigsvar_name, gsigsvar_fmtAtom, gsigsvar_fmtBindAtom,
    gseval_state_error_view, gseval_state_implementation_failure_view, gseval_state_whnf_view,
    gswhnf_natural_view, gswhnf_rune_view, gswhnf_constr_view, gswhnf_record_view, gswhnf_function_view,
    gsvalue_constr
  ) where

import Control.Exception (SomeException, try, throwIO, fromException)

import qualified Data.Map as Map

import GSI.Util (Pos, StackTrace(..), fmtPos, gshere)
import GSI.Syn (GSVar, gsvar, varName, fmtVarAtom, fmtVarBindAtom)
import GSI.Error (GSError(..), GSException(..), fmtError)
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (OPort)
import GSI.Value (GSValue(..), GSExpr, GSIntArg(..), GSIntExpr(..), GSBCO(..), GSExternal(..), gslambda_value, gsconstr, gsrecord, gsimpprim, gsthunk_w, gsintthunk_w, gsapply_w, gsfield_w, gsundefined_value, gsundefined_value_w, gsexternal, gsav, gsae, gsargexpr_w, gsvFmt, gsvCode, bcoCode, whichExternal)
import GSI.ThreadType (Thread)
import GSI.Thread (createThread, execMainThread)
import API (apiImplementationFailure)
import GSI.Eval (evalSync)
import GSI.Functions (gslist, gsstring, gsapiEval, gsapiEvalPos, gsapiEvalExternal, gsapiEvalList)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.ByteCode (gsbcarg, gsbcarg_w, gsbclfield_w, gsbcforce, gsbcevalexternal, gsbcevalnatural, gsbcrehere_w, gsbcapply, gsbcapply_w, gsbcnatural_w, gsbcenter, gsbcenter_w, gsbcexternal, gsbcconstr, gsbcconstr_view, gsbcundefined_w, gsbcruntimetypeerror, gsbcimplementationfailure, gsbcprim_w, gsbcimpprim, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit, gsbcdiscardpattern_w, gsbcvarpattern_w, gsbcviewpattern_w)
import GSI.BCFunctions (gsbcevalpos, gsbcevallist, gsbcevalstring, gsbcevalmap)
import GSI.String (gsbcstringlit)

gsi_monad = GSRecord $gshere $ Map.fromList [
    (gsvar ">>=", $gslambda_value $ \ a -> $gsbcarg $ \ f -> $gsbcimpfor $ do
        x <- $gsbcimpbind $ $gsav a
        $gsbcimpbody $ $gsae $ $gsbcapply f [ $gsav x ]
    ),
    (gsvar "unit", $gslambda_value $ \ x -> $gsbcimpfor $ $gsbcimpunit $ $gsav x)
  ]

gsigsinject = $gslambda_value $ \ v -> $gsbcexternal v

gsigsthunk = $gsimpprim $ \ msg pc pos0 th (pos1v :: GSValue) (ev :: GSValue) -> do
    pos1 <- gsapiEvalPos msg pc pos0 pos1v
    e <- gsapiEvalExternal msg pc pos0 ev :: IO GSExpr
    r <- gsthunk_w pos1 e
    return $ gsexternal r

gsigsintthunk = $gsimpprim $ \ msg pc pos0 th (pos1v :: GSValue) (ev :: GSValue) -> do
    pos1 <- gsapiEvalPos msg pc pos0 pos1v
    e <- gsapiEvalExternal msg pc pos0 ev :: IO GSIntExpr
    r <- gsintthunk_w pos1 e
    return $ gsexternal r

gsigsiae = $gslambda_value $ \ posv -> $gsbcarg $ \ ev ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav ev) $ \ e ->
        $gsbcexternal $ GSIArgExpr pos e

gsigsiagv = $gslambda_value $ \ gv ->
    $gsbcevalexternal ($gsav gv) $ \ g ->
        $gsbcexternal $ GSIArgGVar g

gsigsintbcwithhere = $gslambda_value $ \ posv -> $gsbcarg $ \ kv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav kv) $ \ k ->
        $gsbcexternal $ GSIntWithHere pos k

gsigsintbcopenexpr = $gslambda_value $ \ posv -> $gsbcarg $ \ ev ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav ev) $ \ e ->
        $gsbcexternal $ GSIntOpenExpr pos e

gsigsintbcgenter = $gslambda_value $ \ posv -> $gsbcarg $ \ xv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav xv) $ \ x ->
        $gsbcexternal $ GSIntGEnter pos x

gsigsintbcfenter = $gslambda_value $ \ posv -> $gsbcarg $ \ vv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcexternal $ GSIntFEnter pos v

gsigsintbcgapply = $gslambda_value $ \ posv -> $gsbcarg $ \ fv -> $gsbcarg $ \ asv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav fv) $ \ f ->
        $gsbcevallist ($gsav asv) $ \ avs -> $gsbcevalmap $gsbcevalexternal (map $gsav avs) $ \ as ->
            $gsbcexternal $ GSIntGApply pos f as

gsigsintbceapply = $gslambda_value $ \ posv -> $gsbcarg $ \ fv -> $gsbcarg $ \ asv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav fv) $ \ f ->
        $gsbcevallist ($gsav asv) $ \ avs -> $gsbcevalmap $gsbcevalexternal (map $gsav avs) $ \ as ->
            $gsbcexternal $ GSIntEApply pos f as

gsigsintbcnatural = $gslambda_value $ \ posv -> $gsbcarg $ \ nv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalnatural ($gsav nv) $ \ n ->
        $gsbcexternal $ GSIntGEnter pos (GSNatural n)

gsigsintbcundefined = $gslambda_value $ \ posv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcexternal $ GSIntUndefined pos

gsigsintbcinsufficientcases = $gslambda_value $ \ posv ->
    $gsbcevalpos ($gsav posv) $ \ pos ->
        $gsbcexternal $ GSIntBEnter pos $ $gsbcarg $ \ x -> gsbcprim_w pos gspriminsufficientcases x

gsigsintbcvarpattern = $gslambda_value $ \ posv -> $gsbcarg $ \ xv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav xv) $ \ x ->
        $gsbcexternal $ GSIntBEnter pos $ gsbcvarpattern_w pos x

gsigsintbcdiscardpattern = $gslambda_value $ \ posv ->
    $gsbcevalpos ($gsav posv) $ \ pos ->
        $gsbcexternal $ GSIntBEnter pos $ gsbcdiscardpattern_w pos

gsigsapply :: GSValue
gsigsapply = $gsimpprim gsiprimgsapply

gsiprimgsapply :: OPort Message -> Maybe ProfCounter -> Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimgsapply msg pc pos t fv asv = do
    f <- gsapiEvalExternal msg pc pos fv :: IO GSValue
    as <- (gsapiEvalList msg pc pos asv >>= mapM (\ av -> gsapiEvalExternal msg pc pos av >>= \ a -> return a)) :: IO [GSValue]
    gsexternal <$> gsapply_w pos f as

gsigsbcarg = $gslambda_value $ \ posv -> $gsbcarg $ \ kv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal (gsbcarg_w pos $ \ x ->
        $gsbcevalexternal ($gsae $ $gsbcapply kv [ $gsav $ gsexternal x ]) $ \ e -> e
    )

gsigsbcrehere = $gslambda_value $ \ posv -> $gsbcarg $ \ kv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcevalexternal ($gsav kv) $ \ k ->
        $gsbcexternal (gsbcrehere_w pos k)

gsigsbclfield = $gslambda_value $ \ posv -> $gsbcarg $ \ fv -> $gsbcarg $ \ rv -> $gsbcarg $ \ kv ->
    $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcevalexternal ($gsav fv) $ \ f ->
    $gsbcevalexternal ($gsav rv) $ \ r ->
        $gsbcexternal (gsbclfield_w pos f r $ \ x ->
            $gsbcevalexternal ($gsae $ $gsbcapply kv [ $gsav $ gsexternal x ]) $ \ e -> e
        )

gsigsundefined = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal $ gsundefined_value_w pos

gsigsbcundefined = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal $ gsbcundefined_w pos

gsigsbcnatural = $gslambda_value $ \ posv -> $gsbcarg $ \ nv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcforce ($gsav nv) $ \ nv0 -> case nv0 of
        GSNatural n -> $gsbcexternal $ gsbcnatural_w pos n
        _ -> $gsbcimplementationfailure $ "gsigsbcnatural " ++ gsvCode nv0 ++ " next"

gsigsbcenter = $gslambda_value $ \ posv -> $gsbcarg $ \ vv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcexternal $ gsbcenter_w pos v

gsigsbcapply = $gslambda_value $ \ posv -> $gsbcarg $ \ fv -> $gsbcarg $ \ avsv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav fv) $ \ f ->
        $gsbcevallist ($gsav avsv) $ \ avs ->
            foldr (\ av f as0 k -> $gsbcevalexternal ($gsav av) $ \ a -> f (as0 . (a:)) k) (\ d k -> k (d [])) avs id $ \ as ->
                $gsbcexternal (gsbcapply_w pos f as)

gsigsbcinsufficientcases = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal ($gsbcarg $ \ x -> gsbcprim_w pos gspriminsufficientcases x)

gsigsbcdiscardpattern = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal (gsbcdiscardpattern_w pos)

gsigsbcvarpattern = $gslambda_value $ \ posv -> $gsbcarg $ \ vv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcexternal (gsbcvarpattern_w pos v)

gsigsbcviewpattern = $gslambda_value $ \ posv -> $gsbcarg $ \ vv -> $gsbcarg $ \ asvv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcevallist ($gsav asvv) $ \ asv -> $gsbcevalmap $gsbcevalexternal (map $gsav asv) $ \ as ->
            $gsbcexternal (gsbcviewpattern_w pos v as)

gsigsav = $gslambda_value $ \ vv -> $gsbcevalexternal ($gsav vv) $ \ v -> $gsbcexternal ($gsav v)

gsigsae = $gslambda_value $ \ posv -> $gsbcarg $ \ ev ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav ev) $ \ e -> $gsbcexternal (gsargexpr_w pos e)

gsigsvar = $gslambda_value $ \ v -> $gsbcevalstring ($gsav v) $ \ v_s -> $gsbcexternal (gsvar v_s)

gsigsvar_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ vv -> $gsbcevalexternal ($gsav vv) $ \ v ->
    $gsbcapply sk [ $gsav ($gsstring (varName v)) ]

gsieval_sync = $gslambda_value $ \ vv -> $gsbcevalexternal ($gsav vv) $ \ (v :: GSValue) -> $gsbcimpprim $ \ msg pc pos t -> w msg pc pos v where
    w :: OPort Message -> Maybe ProfCounter -> Pos -> GSValue -> IO GSValue
    w _ _ _ (GSError e) = return $ $gsconstr (gsvar "error") [ gsexternal e ]
    w _ _ _ (GSImplementationFailure pos err) = return $ $gsconstr (gsvar "implementation-failure") [ gsexternal pos, $gsstring err ]
    w _ _ _ v@GSNatural{} = return $ $gsconstr (gsvar "whnf") [ gsexternal v ]
    w _ _ _ v@GSRune{} = return $ $gsconstr (gsvar "whnf") [ gsexternal v ]
    w _ _ _ v@GSConstr{} = return $ $gsconstr (gsvar "whnf") [ gsexternal v ]
    w _ _ _ v@GSRecord{} = return $ $gsconstr (gsvar "whnf") [ gsexternal v ]
    w _ _ _ v@GSClosure{} = return $ $gsconstr (gsvar "whnf") [ gsexternal v ]
    w msg pc pos (GSThunk ts) = do
        v' <- evalSync msg pc [StackTrace pos []] ts
        w msg pc pos v'
    w _ _ _ v = $apiImplementationFailure $ "gsieval_sync " ++ gsvCode v ++ " next"

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: OPort Message -> Maybe ProfCounter -> Pos -> Thread -> GSValue -> IO GSValue
gsiprimcreateThread msg pc pos t vv = do
    v <- gsapiEvalExternal msg pc pos vv
    t <- createThread msg pc pos v Nothing
    return $ GSExternal $ toExternal t

gsiexecMainThread :: GSValue
gsiexecMainThread = $gsimpprim gsiprimexecMainThread

gsiprimexecMainThread :: OPort Message -> Maybe ProfCounter -> Pos -> Thread -> GSValue -> IO GSValue
gsiprimexecMainThread msg pc pos tself tprogv = do
    tprog <- gsapiEvalExternal msg pc pos tprogv
    mb <- try $ execMainThread tprog :: IO (Either SomeException ())
    case mb of
        Left e | Just e' <- fromException e -> case e' :: GSException of
            GSExcError e -> throwIO $ GSExcError e
            GSExcImplementationFailure pos s -> throwIO $ GSExcImplementationFailure pos s
            _ -> $apiImplementationFailure $ "execMainThread threw unknown exception " ++ show e' ++ " next"
        Left e -> $apiImplementationFailure $ "execMainThread threw unknown exception " ++ show e ++ " next"
        Right () -> $apiImplementationFailure $ "gsiexecMainThread next"

gsigsfmtError = $gslambda_value $ \ e -> $gsbcforce ($gsav e) $ \ ev -> case ev of
    GSExternal ee
        | Just hse <- fromExternal ee -> $gsbcstringlit (fmtError hse)
        | otherwise -> $gsbcimplementationfailure $ "gsigsfmtError " ++ whichExternal ee ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsfmtError " ++ gsvCode ev ++ " next"

gsigsvar_compare = $gslambda_value $ \ v0 -> $gsbcarg $ \ v1 ->  $gsbcforce ($gsav v0) $ \ v0v -> $gsbcforce ($gsav v1) $ \ v1v -> case (v0v, v1v) of
    (GSExternal v0e, GSExternal v1e)
        | Just v0hsv <- fromExternal v0e, Just v1hsv <- fromExternal v1e -> case compare (v0hsv :: GSVar) (v1hsv :: GSVar) of
            LT -> $gsbcconstr (gsvar "lt") []
            EQ -> $gsbcconstr (gsvar "eq") []
            GT -> $gsbcconstr (gsvar "gt") []
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_compare " ++ whichExternal v0e ++ ' ' : whichExternal v1e ++ " next"
    (GSExternal _, _) -> $gsbcruntimetypeerror "gsvar.<=> _ •" (gsvFmt v1v "") "(GSExternal GSVar)"
    (_, _) -> $gsbcruntimetypeerror "gsvar.<=> • _" (gsvFmt v0v "") "(GSExternal GSVar)"

gsigsvar_eq = $gslambda_value $ \ v0 -> $gsbcarg $ \ v1 ->  $gsbcforce ($gsav v0) $ \ v0v -> $gsbcforce ($gsav v1) $ \ v1v -> case (v0v, v1v) of
    (GSExternal v0e, GSExternal v1e)
        | Just v0hsv <- fromExternal v0e, Just v1hsv <- fromExternal v1e -> case (==) (v0hsv :: GSVar) (v1hsv :: GSVar) of
            True -> $gsbcconstr (gsvar "true") []
            False -> $gsbcconstr (gsvar "false") []
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_eq " ++ whichExternal v0e ++ ' ' : whichExternal v1e ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsvar_eq " ++ gsvCode v0v ++ ' ' : gsvCode v1v ++ " next"

gsigsvar_name = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ vv -> case vv of
    GSExternal ve
        | Just vhsv <- fromExternal ve -> $gsbcstringlit (varName vhsv)
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_name " ++ whichExternal ve ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsvar_name " ++ gsvCode vv ++ " next"

gsigsvar_fmtAtom = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ vv -> case vv of
    GSExternal ve
        | Just vhsv <- fromExternal ve -> $gsbcstringlit (fmtVarAtom vhsv "")
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ whichExternal ve ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ gsvCode vv ++ " next"

gsigsvar_fmtBindAtom = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ vv -> case vv of
    GSExternal ve
        | Just vhsv <- fromExternal ve -> $gsbcstringlit (fmtVarBindAtom vhsv "")
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ whichExternal ve ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ gsvCode vv ++ " next"

gseval_state_error_view = $gsbcconstr_view "error"
gseval_state_implementation_failure_view = $gsbcconstr_view "implementation-failure"
gseval_state_whnf_view = $gsbcconstr_view "whnf"

gswhnf_natural_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    (GSExternal e) | Just (GSNatural n) <- fromExternal e -> $gsbcapply sk [ $gsav $ GSNatural n ]
    _ -> $gsbcenter ek

gswhnf_rune_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSRune r) <- fromExternal e -> $gsbcapply sk [ $gsav $ GSRune r ]
    _ -> $gsbcenter ek

gswhnf_constr_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSConstr pos c as) <- fromExternal e -> $gsbcapply sk [
        $gsav $ gsexternal pos,
        $gsav $ gsexternal c,
        $gsav $ $gslist $ map gsexternal as
      ]
    _ -> $gsbcenter ek

gswhnf_record_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSRecord pos fs) <- fromExternal e -> $gsbcapply sk [
        $gsav $ gsexternal pos,
        $gsav $ $gslist $ map (\ (f, v) -> $gsrecord [ (gsvar "0", gsexternal f), (gsvar "1", gsexternal v) ]) $ Map.toList fs
      ]
    _ -> $gsbcenter ek

gswhnf_function_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSClosure _ GSLambda{}) <- fromExternal e -> $gsbcapply sk [ $gsav v ]
    _ -> $gsbcenter ek

gsvalue_constr = $gslambda_value $ \ posv -> $gsbcarg $ \ vv -> $gsbcarg $ \ asv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcevallist ($gsav asv) $ \ as0 -> $gsbcevalmap $gsbcevalexternal (map $gsav as0) $ \ as ->
            $gsbcexternal (GSConstr pos v as)
