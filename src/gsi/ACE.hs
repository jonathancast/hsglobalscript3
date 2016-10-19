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

aceEnter pos fn st = aceUnimpl_w $gshere "aceEnter next" st

aceUnimpl_w pos err [] = return ()
aceUnimpl_w pos err (StUpdate mv:st) = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection $ GSImplementationFailure pos err, Just b)
        _ -> return (GSTSIndirection $ GSImplementationFailure pos err, Nothing)
    maybe (return ()) wakeup mbb
    aceUnimpl_w pos err st
aceUnimpl_w pos err (_:st) = aceUnimpl_w pos err st

stackCode :: Stack a -> String
stackCode StApp{} = "StApp"
stackCode StUpdate{} = "StUpdate"
