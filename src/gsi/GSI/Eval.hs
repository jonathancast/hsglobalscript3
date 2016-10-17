{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval) where

import Control.Concurrent (modifyMVar)

import GSI.Value (GSValue(..), GSThunkState(..), gsimplementationFailure, gsvCode, gstsCode)
import GSI.Result (GSError(..), GSResult(..), implementationFailure)

eval :: GSValue a -> IO (GSResult a)
eval (GSUndefined pos) = return $ GSError (GSErrUnimpl pos)
eval (GSThunk mv) = modifyMVar mv $ \ st -> case st of
    _ -> do
        let err = $gsimplementationFailure $ "eval (thunk: " ++ gstsCode st ++ ") next"
        return (GSTSIndirection err, GSIndirection err)
eval v = return $ $implementationFailure $ "eval " ++ gsvCode v ++ " next"
