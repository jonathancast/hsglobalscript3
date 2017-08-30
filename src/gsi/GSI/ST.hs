{-# LANGUAGE TemplateHaskell #-}
module GSI.ST (gsstrun, gsstgetvar) where

import GSI.Util (Pos, StackTrace(..), gshere)
import GSI.Value (GSValue(..), gslambda_value, gsprim, gsimpprim, gsundefined_value, gsimplementationfailure, gsav, gsvCode)
import GSI.ByteCode (gsbcforce, gsbcapply)
import GSI.Eval (evalSync)
import GSI.ThreadType (Thread, ThreadData(..), ThreadState(..), threadStateCode)
import GSI.Thread (createThread, waitThread, createPromise, readPromise)
import API (apiImplementationFailure)

gsstrun :: GSValue
gsstrun = $gsprim gsprim_st_run :: GSValue

gsprim_st_run :: Pos -> GSValue -> IO GSValue
gsprim_st_run pos a = do
    pr <- createPromise
    t <- createThread pos stThreadData a (Just pr)
    st <- waitThread t
    case st of
        ThreadStateUnimpl pos err -> return $ GSImplementationFailure pos err
        ThreadStateError err -> return $ GSError err
        ThreadStateImplementationFailure pos err -> return $ GSImplementationFailure pos err
        ThreadStateSuccess -> readPromise pr
        _ -> return $ $gsimplementationfailure $ "st.run (state is " ++ threadStateCode st ++ ") next"

gsstgetvar = $gslambda_value $ \ v -> $gsbcforce ($gsav v) $ \ v0 -> $gsbcapply ($gsimpprim gsprim_st_get_var) [ $gsav v0 ]

gsprim_st_get_var :: Pos -> Thread -> GSValue -> IO GSValue
gsprim_st_get_var pos t v = $apiImplementationFailure $ "gsprim_st_get_var pos t " ++ gsvCode v ++ " next"

stThreadData = ThreadData{
    component = Nothing,
    threadTypeName = "stThreadData"
  }
