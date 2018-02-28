{-# LANGUAGE TemplateHaskell, ExistentialQuantification, ScopedTypeVariables #-}
module GSI.GSI (gsigsinject, gsigsthunk, gsigsapply, gsigsundefined, gsigsav, gsigsae, gsigsbcarg, gsigsbcwithhere, gsigsbclfield, gsigsbcapply, gsigsbcundefined, gsigsbcnatural, gsigsbcenter, gsigsbcinsufficientcases, gsigsbcvarpattern, gsigsvar, gsigsevalSync, gsicreateThread, gsiexecMainThread, GSIThread(..), gsigsfmtError, gsiThreadData, gsigsiThreadData, gsigsvar_eq, gsigsvar_compare, gsigsvar_name, gsigsvar_fmtAtom, gsvalue_error_view, gsvalue_natural_view, gsvalue_function_view, gsvalue_thunk_view) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, try, throwIO, fromException)

import Component.Monad (mvarContents)

import GSI.Util (Pos, StackTrace(..), fmtPos, gshere)
import GSI.Syn (GSVar, gsvar, varName, fmtVarAtom)
import GSI.Error (GSError(..), GSException(..), fmtError)
import GSI.Value (GSValue(..), GSArg, GSExpr, GSBCO(..), GSExternal(..), gslambda_value, gsconstr, gsimpprim, gsthunk_w, gsundefined_value, gsundefined_value_w, gsexternal, gsav, gsae, gsargexpr_w, gsvCode, bcoCode, whichExternal)
import GSI.ThreadType (Thread, ThreadData(..), ThreadException(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import API (apiImplementationFailure)
import GSI.Eval (evalSync)
import GSI.Functions (gslist, gsapiEvalExternal, gsapiEvalList)
import GSI.CalculusPrims (gspriminsufficientcases)
import GSI.ByteCode (gsbcarg, gsbcarg_w, gsbclfield_w, gsbcforce, gsbcforce_w, gsbcevalexternal, gsbcwithhere_w, gsbcapply, gsbcapply_w, gsbcnatural_w, gsbcenter, gsbcexternal, gsbcenter_w, gsbcconstr, gsbcundefined_w, gsbcimplementationfailure, gsbcprim_w, gsbcimpprim, gsbcvarpattern_w)
import GSI.Env (GSEnvArgs(..))
import GSI.StdLib (gsbcevalpos, gsapiEvalPos, gsbcevalstring)
import GSI.String (gsbcstringlit)

gsigsinject = $gslambda_value $ \ v -> $gsbcexternal v

gsigsthunk = $gsimpprim $ \ pos0 th (pos1v :: GSValue) (ev :: GSValue) -> do
    pos1 <- gsapiEvalPos pos0 pos1v
    e <- gsapiEvalExternal pos0 ev :: IO GSExpr
    r <- gsthunk_w pos1 e
    return $ gsexternal r

gsigsapply :: GSValue
gsigsapply = $gsimpprim gsiprimgsapply

gsiprimgsapply :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimgsapply pos t fv asv = do
    f <- gsapiEvalExternal pos fv :: IO GSValue
    as <- (gsapiEvalList pos asv >>= mapM (\ av -> gsapiEvalExternal pos av >>= \ a -> return a)) :: IO [GSValue]
    $apiImplementationFailure $ "gsiprimgsapply next"

gsigsbcarg = $gslambda_value $ \ posv -> $gsbcarg $ \ kv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal (gsbcarg_w pos $ \ x ->
        $gsbcevalexternal ($gsae $ $gsbcapply kv [ $gsav $ gsexternal x ]) $ \ e -> e
    )

gsigsbcwithhere = $gslambda_value $ \ posv -> $gsbcarg $ \ kv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal (gsbcwithhere_w pos $ \ here ->
        $gsbcevalexternal ($gsae $ $gsbcapply kv [ $gsav $ gsexternal here ]) $ \ e -> e
    )

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
        gsbcevallist_w $gshere ($gsav avsv) $ \ avs ->
            foldr (\ av f as0 k -> $gsbcevalexternal ($gsav av) $ \ a -> f (as0 . (a:)) k) (\ d k -> k (d [])) avs id $ \ as ->
                $gsbcexternal (gsbcapply_w pos f as)

gsbcevallist_w :: Pos -> GSArg -> ([GSValue] -> GSExpr) -> GSExpr
gsbcevallist_w pos a k = w a id where
    w a d = gsbcforce_w pos a $ \ v -> case v of
        GSConstr _ c [] | c == gsvar "nil" -> k (d [])
        GSConstr _ c [ v0, v1 ] | c == gsvar ":" -> w ($gsav v1) (d . (v0:))
        GSConstr _ c as -> $gsbcimplementationfailure $ "gsbcevallist " ++ fmtVarAtom c " next"
        _ -> $gsbcimplementationfailure $ "gsbcevallist " ++ gsvCode v ++ " next"

gsigsbcinsufficientcases = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal ($gsbcarg $ \ x -> gsbcprim_w pos gspriminsufficientcases x)

gsigsbcvarpattern = $gslambda_value $ \ posv -> $gsbcarg $ \ vv ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav vv) $ \ v ->
        $gsbcexternal (gsbcvarpattern_w pos v)

gsigsav = $gslambda_value $ \ vv -> $gsbcevalexternal ($gsav vv) $ \ v -> $gsbcexternal ($gsav v)

gsigsae = $gslambda_value $ \ posv -> $gsbcarg $ \ ev ->
    $gsbcevalpos ($gsav posv) $ \ pos -> $gsbcevalexternal ($gsav ev) $ \ e -> $gsbcexternal (gsargexpr_w pos e)

gsigsvar = $gslambda_value $ \ v -> $gsbcevalstring ($gsav v) $ \ v_s -> $gsbcexternal (gsvar v_s)

gsigsevalSync = $gsimpprim $ \ pos t vv -> do
    v <- gsapiEvalExternal pos vv :: IO GSValue
    v' <- case v of
        GSThunk ts -> evalSync [StackTrace pos []] ts
        _ -> return v
    return $ gsexternal v'

gsicreateThread :: GSValue
gsicreateThread = $gsimpprim gsiprimcreateThread

gsiprimcreateThread :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimcreateThread pos t tdv vv = do
    td <- gsapiEvalExternal pos tdv
    v <- gsapiEvalExternal pos vv
    t <- createThread pos td v Nothing
    return $ GSExternal $ toExternal t

gsiexecMainThread :: GSValue
gsiexecMainThread = $gsimpprim gsiprimexecMainThread

gsiprimexecMainThread :: Pos -> Thread -> GSValue -> IO GSValue
gsiprimexecMainThread pos tself tprogv = do
    tprog <- gsapiEvalExternal pos tprogv
    mb <- try $ execMainThread tprog :: IO (Either SomeException ())
    case mb of
        Left e | Just e' <- fromException e -> case e' :: GSException of
            GSExcUndefined st -> throwIO $ TEError $ GSErrUnimpl st
            GSExcInsufficientCases pos s -> throwIO $ TEError $ GSErrInsufficientCases pos s
            GSExcError pos s -> throwIO $ TEError $ GSErrError pos s
            _ -> $apiImplementationFailure $ "execMainThread threw unknown exception " ++ show e' ++ " next"
        Left e -> $apiImplementationFailure $ "execMainThread threw unknown exception " ++ show e ++ " next"
        Right () -> $apiImplementationFailure $ "gsiexecMainThread next"

gsigsiThreadData :: GSValue
gsigsiThreadData = $gsimpprim gsiprimgsiThreadData

gsiprimgsiThreadData :: Pos -> Thread -> GSValue -> IO GSValue
gsiprimgsiThreadData pos t args = do
    as <- newMVar $ GSEnvArgs $ args
    gsiprimthreadData pos t (gsiThreadData GSIThread{ envArgs = as })

gsiprimthreadData :: Pos -> Thread -> ThreadData -> IO GSValue
gsiprimthreadData pos t td = do
    return $ GSExternal $ toExternal td

data GSIThread = GSIThread{
    envArgs :: MVar GSEnvArgs
  }

gsiThreadData :: GSIThread -> ThreadData
gsiThreadData d = ThreadData{
    component = fetchThreadDataComponent gsiThreadComponents d,
    threadTypeName = fmtPos $gshere "GSIThread"
  }

gsiThreadComponents =
    insertThreadDataComponent (\d -> mvarContents (envArgs d)) $
    emptyThreadDataComponents

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
    _ -> $gsbcimplementationfailure $ "gsigsvar_compare " ++ gsvCode v0v ++ ' ' : gsvCode v1v ++ " next"

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

gsvalue_error_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    (GSExternal e) | Just (GSError err) <- fromExternal e -> $gsbcapply sk [ $gsav $ gsexternal err ]
    _ -> $gsbcenter ek

gsvalue_natural_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    (GSExternal e) | Just (GSNatural n) <- fromExternal e -> $gsbcapply sk [ $gsav $ GSNatural n ]
    _ -> $gsbcenter ek

gsvalue_function_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSClosure _ GSLambda{}) <- fromExternal e -> $gsbcapply sk [ $gsav v ]
    _ -> $gsbcenter ek

gsvalue_thunk_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just GSThunk{} <- fromExternal e -> $gsbcapply sk [ $gsav v ]
    _ -> $gsbcenter ek
