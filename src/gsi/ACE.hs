{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (Stack(..), aceEnter) where

import Control.Monad (forM_)

import Control.Concurrent (MVar, modifyMVar)

import GSI.Util (gshere)
import GSI.RTS (wakeup)
import GSI.Value (GSValue(..), GSThunkState(..))

data Stack a
  = StApp [GSValue a]
  | StUpdate (MVar (GSThunkState a))

aceEnter pos fn stack = aceUnimpl_w $gshere "aceEnter next" stack

aceUnimpl_w pos err = aceThrow (GSImplementationFailure pos err)

aceThrow err stack= forM_ stack $ \ cont -> case cont of
    StUpdate mv -> aceUpdate mv err
    _ -> return ()

aceUpdate mv v = do
    mbb <- modifyMVar mv $ \ s -> case s of
        GSTSStack b -> return (GSTSIndirection v, Just b)
        _ -> return (GSTSIndirection v, Nothing)
    maybe (return ()) wakeup mbb

stackCode :: Stack a -> String
stackCode StApp{} = "StApp"
stackCode StUpdate{} = "StUpdate"
