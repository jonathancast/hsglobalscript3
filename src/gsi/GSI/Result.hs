{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Result (throwGSerror) where

import Control.Exception (throw)

import GSI.Util (Pos, fmtPos, gshere)
import GSI.Error (GSError(..), GSException(..))

throwGSerror (GSErrUnimpl pos) = throw $ GSExcUndefined pos
throwGSerror err = throw $ GSExcImplementationFailure $gshere $ "throwGSerror (" ++ show err ++ ") next"
