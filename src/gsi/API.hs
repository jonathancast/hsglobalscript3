{-# LANGUAGE TemplateHaskell #-}
module API (apiCallBCO) where

import Control.Exception (throwIO)

import GSI.Util (Pos, gshere)
import GSI.Value (GSValue)
import {-# SOURCE #-} GSI.ByteCode (GSBCO(..), bcoCode)
import GSI.ThreadType (Thread, ThreadException(..))

apiCallBCO :: Pos -> GSBCO -> Thread -> IO GSValue
apiCallBCO pos bco t = throwIO $ TEImplementationFailure $gshere $ "apiCallBCO " ++ bcoCode bco ++ " next"
