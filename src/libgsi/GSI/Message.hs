{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Message (Message(..), msgCode) where

import GSI.Util (StackTrace)

data Message
  = MsgProfile StackTrace

msgCode MsgProfile{} = "MsgProfile"
