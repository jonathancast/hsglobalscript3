{-# LANGUAGE TemplateHaskell #-}
module GSI.Env (GSEnvArgs(..), gsenvGetArgs, gsfileStat) where

import Component.Monad (getM)

import GSI.Util (Pos)
import GSI.Value (GSValue, gsimpprim, gsundefined)
import GSI.ThreadType (ThreadDataComponent(..), component, threadTypeName)
import GSI.Thread (Thread, withThreadData)
import API (apiImplementationFailure)

gsenvGetArgs = $gsimpprim gsprimenvGetArgs :: GSValue

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

gsfileStat :: GSValue
gsfileStat = $gsimpprim gsprimfileStat

gsprimfileStat :: Pos -> Thread -> GSValue -> IO GSValue
gsprimfileStat pos t fn = do
    $apiImplementationFailure $ "gsprimenvFileStat next"
