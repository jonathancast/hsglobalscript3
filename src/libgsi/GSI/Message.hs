{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Message (Message(..), GSMessageClass(..), mkMessage, msgCode, msgClassCode) where

import GSI.Util (StackTrace)
import {-# SOURCE #-} GSI.Value (GSValue)

data Message
  = MsgProfile StackTrace
  | MsgInfo StackTrace GSValue

data GSMessageClass
  = GSInfo

mkMessage :: StackTrace -> GSMessageClass -> GSValue -> Message
mkMessage st GSInfo v = MsgInfo st v

msgCode MsgProfile{} = "MsgProfile"
msgCode MsgInfo{} = "MsgInfo"

msgClassCode GSInfo = "GSInfo"
