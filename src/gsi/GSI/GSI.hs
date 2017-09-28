{-# LANGUAGE TemplateHaskell, ExistentialQuantification, ScopedTypeVariables #-}
module GSI.GSI (gsigsinject, gsigsthunk, gsigsapply, gsigsundefined, gsigsvar, gsigsevalSync, gsicreateThread, gsiexecMainThread, GSIThread(..), gsigsfmtError, gsiThreadData, gsigsiThreadData, gsigsvar_compare, gsigsvar_fmtAtom, gsvalue_error_view, gsvalue_function_view, gsvalue_thunk_view) where

import Control.Concurrent.MVar (MVar, newMVar)
import Control.Exception (SomeException, try, throwIO, fromException)

import Component.Monad (mvarContents)

import GSI.Util (Pos, StackTrace(..), fmtPos, gshere)
import GSI.Syn (GSVar, gsvar, fmtVarAtom)
import GSI.Error (GSError(..), GSException(..), fmtError)
import GSI.Value (GSValue(..), GSExpr, GSBCO(..), GSExternal(..), gslambda_value, gsconstr, gsimpprim, gsundefined_value, gsundefined_value_w, gsexternal, gsav, gsae, gsvCode, bcoCode, whichExternal)
import GSI.ThreadType (Thread, ThreadData(..), ThreadException(..), fetchThreadDataComponent, insertThreadDataComponent, emptyThreadDataComponents)
import GSI.Thread (createThread, execMainThread)
import API (apiImplementationFailure)
import GSI.Eval (evalSync)
import GSI.Functions (gslist, gsapiEvalExternal, gsapiEvalList)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcapply, gsbcenter, gsbcexternal, gsbcconstr, gsbcimplementationfailure, gsbcimpprim, gsbcconstr_view)
import GSI.Env (GSEnvArgs(..))
import GSI.StdLib (gsbcevalpos, gsapiEvalPos, gsbcevalstring)
import GSI.String (gsbcstringlit)

gsigsinject = $gslambda_value $ \ v -> $gsbcexternal v

gsigsthunk = $gsimpprim $ \ pos th (posv :: GSValue) (ev :: GSValue) -> do
    posv1 <- gsapiEvalPos pos posv
    ev1 <- gsapiEvalExternal pos ev :: IO GSExpr
    $apiImplementationFailure $ "gsigsthunk next" :: IO GSValue

gsigsapply :: GSValue
gsigsapply = $gsimpprim gsiprimgsapply

gsiprimgsapply :: Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsiprimgsapply pos t fv asv = do
    f <- gsapiEvalExternal pos fv :: IO GSValue
    as <- (gsapiEvalList pos asv >>= mapM (\ av -> gsapiEvalExternal pos av >>= \ a -> return a)) :: IO [GSValue]
    $apiImplementationFailure $ "gsiprimgsapply next"

gsigsundefined = $gslambda_value $ \ posv -> $gsbcevalpos ($gsav posv) $ \ pos ->
    $gsbcexternal $ gsundefined_value_w pos

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

gsigsvar_fmtAtom = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ vv -> case vv of
    GSExternal ve
        | Just vhsv <- fromExternal ve -> $gsbcstringlit (fmtVarAtom vhsv "")
        | otherwise -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ whichExternal ve ++ " next"
    _ -> $gsbcimplementationfailure $ "gsigsvar_fmt " ++ gsvCode vv ++ " next"

gsvalue_error_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    (GSExternal e) | Just (GSError err) <- fromExternal e -> $gsbcapply sk [ $gsav $ gsexternal err ]
    _ -> $gsbcenter ek

gsvalue_function_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just (GSClosure _ GSLambda{}) <- fromExternal e -> $gsbcapply sk [ $gsav v ]
    _ -> $gsbcenter ek

gsvalue_thunk_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> case v0 of
    GSExternal e | Just GSThunk{} <- fromExternal e -> $gsbcapply sk [ $gsav v ]
    _ -> $gsbcenter ek
