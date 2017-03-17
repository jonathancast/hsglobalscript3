{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.Env (GSEnvArgs(..), gsenvGetArgs, gsfileStat) where

import Control.Exception (SomeException, try, fromException)

import System.IO.Error (isDoesNotExistError)

import Component.Monad (getM)

import System.Posix.Files (getFileStatus)

import GSI.Util (Pos)
import GSI.Value (GSValue, gsimpprim, gsundefined)
import GSI.ThreadType (ThreadDataComponent(..), component, threadTypeName)
import GSI.Thread (Thread, withThreadData)
import API (apiImplementationFailure)
import GSI.Functions (gsapiEvalString)

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
    fns <- gsapiEvalString pos fn
    mbst <- try $ getFileStatus fns
    case mbst of
        Left e | Just e1 <- fromException e, isDoesNotExistError e1 ->
            return $ $gsundefined
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimenvFileStat " ++ show fns ++ " (stat returned Left (" ++ show e ++ ")) next"
        Right st -> $apiImplementationFailure $ "gsprimenvFileStat " ++ show fns ++ " (stat returned Right) next"
