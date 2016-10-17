{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module ACE (Stack(..), aceEnter) where

import Control.Concurrent (MVar, modifyMVar_)

import GSI.Util (gshere)
import GSI.Value (GSValue(..), GSThunkState(..))

data Stack a
  = StApp [GSValue a]
  | StUpdate (MVar (GSThunkState a))

aceEnter pos fn st = aceUnimpl_w $gshere "aceEnter next" st

aceUnimpl_w pos err [] = return ()
aceUnimpl_w pos err (StUpdate mv:st) = do
    modifyMVar_ mv $ const $ return $ GSTSIndirection $ GSImplementationFailure pos err
    aceUnimpl_w pos err st
aceUnimpl_w pos err (_:st) = aceUnimpl_w pos err st

stackCode :: Stack a -> String
stackCode StApp{} = "StApp"
stackCode StUpdate{} = "StUpdate"
