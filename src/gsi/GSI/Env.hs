{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
module GSI.Env (GSEnvArgs(..), gsenvGetArgs, gsfileStat, gsfileRead, gsprint, gsprintError, gsENOENT_view) where

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import qualified Data.Map as Map

import Control.Exception (SomeException, try, fromException)

import Data.Encoding.UTF8 (UTF8(..))
import System.IO (hPutStrLn, hPutChar, stderr)
import System.IO.Encoding (readFile)
import System.IO.Error (isDoesNotExistError)

import Component.Monad (getM)

import System.Posix.Files (getFileStatus, isDirectory)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (fmtError)
import GSI.Value (GSValue(..), gslambda_value, gsimpprim, gsundefined_value, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)
import GSI.ThreadType (Thread, ThreadDataComponent(..), component, threadTypeName)
import GSI.Thread (withThreadData)
import GSI.Eval (evalSync)
import API (apiImplementationFailure)
import GSI.Functions (gslazystring, gsapiEvalString)

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
        Right st -> return $ GSConstr $gshere (gsvar "right") [ GSRecord $gshere $ Map.fromList [
            (gsvar "is.dir", case isDirectory st of
                False -> GSConstr $gshere (gsvar "false") []
                True -> GSConstr $gshere (gsvar "true") []
            )
          ] ]

gsfileRead :: GSValue
gsfileRead = $gsimpprim gsprimfileRead

gsprimfileRead :: Pos -> Thread -> GSValue -> IO GSValue
gsprimfileRead pos t fn = do
    fns <- gsapiEvalString pos fn
    mbs <- try $ let ?enc = UTF8Strict in readFile fns
    case mbs of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimfileRead " ++ show fns ++ " (readFile returned Left (" ++ show e ++ ")) next"
        Right s -> $gslazystring s

gsprint :: GSValue
gsprint = $gsimpprim gsprimprint

gsprimprint :: Pos -> Thread -> GSValue -> IO GSValue
gsprimprint pos t msg = $apiImplementationFailure "gsprimprint  next"

gsprintError :: GSValue
gsprintError = $gsimpprim gsprimprintError

gsprimprintError :: Pos -> Thread -> GSValue -> IO GSValue
gsprimprintError pos t (GSThunk th) = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprintError pos t v
gsprimprintError pos t (GSImplementationFailure pos1 msg) = do
    hPutStrLn stderr $ fmtPos pos1 $ msg
    return $ $gsundefined_value
gsprimprintError pos t (GSError err) = do
    hPutStrLn stderr $ fmtError err
    return $ $gsundefined_value
gsprimprintError pos t (GSConstr pos1 c []) | c == gsvar "nil" =
    return $ $gsundefined_value
gsprimprintError pos t (GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = do
    hPutChar stderr ch
    gsprimprintError pos t s
gsprimprintError pos t (GSConstr pos1 c [ ch, s ]) | c == gsvar ":" = do
    hPutStrLn stderr $ fmtPos $gshere $ "gsprimprintError (" ++ gsvCode ch ++ " : _) next"
    gsprimprintError pos t s
gsprimprintError pos t (GSConstr pos1 c as) =
    $apiImplementationFailure $ "gsprimprintError " ++ fmtVarAtom c " next"
gsprimprintError pos t msg =
    $apiImplementationFailure $ "gsprimprintError " ++ gsvCode msg ++ " next"

gsENOENT_view :: GSValue
gsENOENT_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ err -> $gsbcconstr_view "ENOENT" ek sk err
