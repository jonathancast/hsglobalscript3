{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
module GSI.Env (GSEnvArgs(..), gsfileStat, gsfileRead, gsprint, gsprintError, gsENOENT_view) where

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import qualified Data.Map as Map

import Control.Exception (SomeException, try, fromException)

import Data.Encoding.UTF8 (UTF8(..))
import System.IO (Handle, hPutStrLn, hPutChar, stdout, stderr)
import System.IO.Encoding (readFile)
import System.IO.Error (isDoesNotExistError)

import System.Posix.Files (getFileStatus, isDirectory)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (fmtError)
import GSI.Value (GSValue(..), gslambda_value, gsimpprim, gsundefined_value, gsvCode)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)
import GSI.ThreadType (Thread, ThreadDataComponent(..))
import GSI.Eval (evalSync)
import API (apiImplementationFailure)
import GSI.Functions (gslazystring, gsapiEvalString)

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
gsprint = $gsimpprim (gsprimprint stdout)

gsprintError :: GSValue
gsprintError = $gsimpprim (gsprimprint stderr)

gsprimprint :: Handle -> Pos -> Thread -> GSValue -> IO GSValue
gsprimprint h pos t (GSThunk th) = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h pos t v
gsprimprint h pos t (GSImplementationFailure pos1 msg) = do
    hPutStrLn h $ fmtPos pos1 $ msg
    return $ $gsundefined_value
gsprimprint h pos t (GSError err) = do
    hPutStrLn h $ fmtError err
    return $ $gsundefined_value
gsprimprint h pos t (GSConstr pos1 c []) | c == gsvar "nil" =
    return $ $gsundefined_value
gsprimprint h pos t (GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = do
    hPutChar h ch
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c [ GSThunk th, s ]) | c == gsvar ":" = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h pos t (GSConstr pos1 c [ v, s ])
gsprimprint h pos t (GSConstr pos1 c [ GSImplementationFailure pos2 msg, s ]) | c == gsvar ":" = do
    hPutStrLn h $ fmtPos pos2 $ msg
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c [ ch, s ]) | c == gsvar ":" = do
    hPutStrLn h $ fmtPos $gshere $ "gsprimprint (" ++ gsvCode ch ++ " : _) next"
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c as) =
    $apiImplementationFailure $ "gsprimprint " ++ fmtVarAtom c " next"
gsprimprint h pos t msg =
    $apiImplementationFailure $ "gsprimprint " ++ gsvCode msg ++ " next"

gsENOENT_view :: GSValue
gsENOENT_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ err -> $gsbcconstr_view "ENOENT" ek sk err
