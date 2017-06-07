{-# LANGUAGE TemplateHaskell #-}
module GSI.ST (gsstrun) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), gsprim, gsundefined_value, gsimplementationfailure, gsvCode)
import GSI.ThreadType (ThreadData(..), ThreadState(..), threadStateCode)
import GSI.Thread (createThread, waitThread, createPromise, readPromise)

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

stThreadData = ThreadData{
    component = Nothing,
    threadTypeName = "stThreadData"
  }
