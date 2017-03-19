{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}
module GSI.Env (GSEnvArgs(..), gsenvGetArgs, gsfileStat, gsprintError, gsENOENT_view) where

import Control.Exception (SomeException, try, fromException)

import System.IO (hPutStrLn, stderr)
import System.IO.Error (isDoesNotExistError)

import Component.Monad (getM)

import System.Posix.Files (getFileStatus)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda, gsimpprim, gsundefined, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)
import GSI.ThreadType (ThreadDataComponent(..), component, threadTypeName)
import GSI.Thread (Thread, withThreadData)
import GSI.Eval (evalSync)
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
            return $ GSConstr $gshere (gsvar "left") [ GSConstr $gshere (gsvar "ENOENT") [ fn ] ]
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimenvFileStat " ++ show fns ++ " (stat returned Left (" ++ show e ++ ")) next"
        Right st -> $apiImplementationFailure $ "gsprimenvFileStat " ++ show fns ++ " (stat returned Right) next"

gsprintError :: GSValue
gsprintError = $gsimpprim gsprimprintError

gsprimprintError :: Pos -> Thread -> GSValue -> IO GSValue
gsprimprintError pos t (GSThunk th) = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprintError pos t v
gsprimprintError pos t (GSImplementationFailure pos1 msg) = do
    hPutStrLn stderr $ fmtPos pos1 $ msg
    return $ $gsundefined
gsprimprintError pos t msg = do
    $apiImplementationFailure $ "gsprimprintError " ++ gsvCode msg ++ " next"

gsENOENT_view :: GSValue
gsENOENT_view = $gslambda $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ err -> $gsbcconstr_view "ENOENT" ek sk err