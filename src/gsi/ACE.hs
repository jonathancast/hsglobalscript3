{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (Stack(..), aceEnter) where

import Control.Monad (forM_)

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..), gsvCode)
import qualified GSI.Value as GSV
import GSI.Result (GSResult(..), stCode)
import {-# SOURCE #-} GSI.Eval (evalSync)

import qualified GSI.Result as GSR

data Stack
  = StApp [GSValue]
  | StUpdate (MVar GSThunkState)

aceEnter pos fn@(GSV.GSError e) stack = aceThrow fn stack
aceEnter pos fn stack = aceUnimpl_w $gshere ("aceEnter (function = " ++ gsvCode fn ++ ") next") stack
-- > aceEnter pos fn stack = do
-- >     st <- evalSync fn
-- >     case st of
-- >         GSR.GSError{} -> aceThrow fn stack
-- >         GSR.GSImplementationFailure{} -> aceThrow fn stack
-- >         _ -> aceUnimpl_w $gshere ("aceEnter (state = " ++ stCode st ++ ") next") stack

aceUnimpl_w pos err = aceThrow (GSV.GSImplementationFailure pos err)

aceThrow err stack= forM_ stack $ \ cont -> case cont of
    StUpdate mv -> aceUpdate mv err
    _ -> return ()

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

stackCode :: Stack -> String
stackCode StApp{} = "StApp"
stackCode StUpdate{} = "StUpdate"
