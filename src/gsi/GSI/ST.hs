{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.ST (gsstrun, gsstrefnew, gsstgetvar, gsstsetvar, gsstrefeq) where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

import qualified Data.Map as Map

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), GSEvalState(..), GSExternal(..), Thread, ThreadState(..), gslambda_value, gsimpprim, gsexternal, gsimplementationfailure, gsav, gsvCode, threadStateCode, whichExternal)
import GSI.ByteCode (gsbcarg, gsbcforce, gsbcevalexternal, gsbcapply, gsbcconstr, gsbcprim)
import GSI.Thread (createThread, waitThread, createPromise, readPromise)
import API (apiImplementationFailure)

gsstrun = $gslambda_value $ \ a -> $gsbcprim gsprim_st_run a

gsprim_st_run :: GSEvalState -> Pos -> GSValue -> IO GSValue
gsprim_st_run evs pos a = do
    pr <- createPromise
    t <- createThread evs pos a (Just pr)
    st <- waitThread t
    case st of
        ThreadStateInvalidProgram err -> return $ GSInvalidProgram err
        ThreadStateError err -> return $ GSError err
        ThreadStateImplementationFailure pos err -> return $ GSImplementationFailure pos err
        ThreadStateSuccess -> readPromise pr
        _ -> return $ $gsimplementationfailure $ "st.run (state is " ++ threadStateCode st ++ ") next"

gsstrefnew = $gsimpprim gsprim_st_ref_new

gsprim_st_ref_new :: GSEvalState -> Pos -> Thread -> GSValue -> IO GSValue
gsprim_st_ref_new evs pos t x = gsexternal . GSSTRef <$> newIORef x

gsstgetvar = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> $gsbcapply ($gsimpprim gsprim_st_get_var) [ $gsav v0 ]

gsprim_st_get_var :: GSEvalState -> Pos -> Thread -> GSValue -> IO GSValue
gsprim_st_get_var evs pos t (GSExternal e) | Just (GSSTRef r) <- fromExternal e = readIORef r
gsprim_st_get_var evs pos t (GSExternal e) = $apiImplementationFailure $ "gsprim_st_get_var pos t " ++ whichExternal e ++ " next"
gsprim_st_get_var evs pos t v = $apiImplementationFailure $ "gsprim_st_get_var pos t " ++ gsvCode v ++ " next"

gsstsetvar = $gslambda_value $ \ x -> $gsbcarg $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> $gsbcapply ($gsimpprim gsprim_st_set_var) [ $gsav x, $gsav v0 ]

gsprim_st_set_var :: GSEvalState -> Pos -> Thread -> GSValue -> GSValue -> IO GSValue
gsprim_st_set_var evs pos t x (GSExternal e) | Just (GSSTRef r) <- fromExternal e = writeIORef r x >> return (GSRecord $gshere Map.empty)
gsprim_st_set_var evs pos t x (GSExternal e) = $apiImplementationFailure $ "gsprim_st_set_var pos t " ++ whichExternal e ++ " next"
gsprim_st_set_var evs pos t x v = $apiImplementationFailure $ "gsprim_st_set_var pos t " ++ gsvCode v ++ " next"

gsstrefeq = $gslambda_value $ \ v0 -> $gsbcarg $ \ v1 ->
    $gsbcevalexternal ($gsav v0) $ \ (v00 :: GSSTRef) -> $gsbcevalexternal ($gsav v1) $ \ v10 -> case v00 == v10 of
        True -> $gsbcconstr (gsvar "true") []
        False -> $gsbcconstr (gsvar "false") []

newtype GSSTRef = GSSTRef (IORef GSValue)
    deriving Eq

instance GSExternal GSSTRef
