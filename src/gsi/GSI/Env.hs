{-# LANGUAGE TemplateHaskell #-}
module GSI.Env (GSEnvArgs(..), gsenvGetArgs) where

import System.IO.Unsafe (unsafePerformIO)

import Component.Monad (getM)

import GSI.Util (Pos)
import GSI.Value (GSValue, GSBCO, gsclosure)
import GSI.ThreadType (ThreadDataComponent(..), component, threadTypeName)
import GSI.Thread (Thread, withThreadData)
import GSI.ByteCode (gsbcimpprim)
import API (apiImplementationFailure)

gsenvGetArgs = unsafePerformIO $ $gsclosure ($gsbcimpprim gsprimenvGetArgs :: GSBCO)

gsprimenvGetArgs :: Pos -> Thread -> IO GSValue
gsprimenvGetArgs pos t = withThreadData t (\ d -> do
    case component d of
        Nothing -> $apiImplementationFailure $ "gsprimenvGetArgs: This thread (" ++ threadTypeName d ++ ") lacks env.args!"
        Just l -> do
            GSEnvArgs args <- getM l -- \<GSEnvArgs\> is the constructor of the \<GSEnvArgs\> newtype; it exists to mark the §gs{env.args} component of the state for \<component\>
            return args
    )

newtype GSEnvArgs = GSEnvArgs GSValue

instance ThreadDataComponent GSEnvArgs where
