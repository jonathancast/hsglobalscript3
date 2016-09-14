{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns #-}
module GSI.Eval (eval) where

import GSI.Value (GSValue(..), gsvCode)
import GSI.Result (GSError(..), GSResult(..), implementationFailure)

eval :: GSValue a -> IO (GSResult a)
eval (GSUndefined pos) = return $ GSError (GSErrUnimpl pos)
eval v = return $ $implementationFailure $ "eval " ++ gsvCode v ++ " next"
