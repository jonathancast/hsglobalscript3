{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (Stack(..), aceEnter) where

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..))

data Stack a
  = StApp [GSValue a]
  | StUpdate (MVar (GSThunkState a))

aceEnter pos fn stack = aceUnimpl_w $gshere "aceEnter next" stack

aceUnimpl_w pos err [] = return ()
aceUnimpl_w pos err (StUpdate mv:st) = do
    aceUpdate mv $ GSImplementationFailure pos err
    aceUnimpl_w pos err st
aceUnimpl_w pos err (_:st) = aceUnimpl_w pos err st

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

stackCode :: Stack a -> String
stackCode StApp{} = "StApp"
stackCode StUpdate{} = "StUpdate"
