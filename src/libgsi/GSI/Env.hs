{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
module GSI.Env (runGSProgram, gsabend, gsfileStat, gsfileRead, gsfile_write, gsdir_read, gsprint, gsprintError, gsENOENT_view) where

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import qualified Data.Map as Map

import Control.Exception (SomeException, try, catch, throwIO, fromException, displayException)

import Data.Encoding.UTF8 (UTF8(..))
import System.IO (Handle, IOMode(..), withFile, hPutStrLn, stdout, stderr)
import System.IO.Encoding (readFile, hPutChar)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitWith)

import System.Posix.Files (getFileStatus, isDirectory)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (GSException(..), fmtInvalidProgram, fmtError)
import GSI.Value (GSValue(..), gslambda_value, gsapply, gsimpprim, gsundefined_value, gsvCode)
import GSI.Functions (gslist, gsstring)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)
import GSI.ThreadType (Thread)
import GSI.Thread (createThread, execMainThread)
import GSI.Eval (evalSync)
import API (apiImplementationFailure)
import GSI.Functions (gslazylist, gslazystring, gsapiEvalString)

runGSProgram a = do
    as <- $gslist . map $gsstring <$> getArgs
    prog <- $gsapply a [as]
    t <- createThread $gshere prog Nothing
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsabend = $gsimpprim $ \ pos t  sv -> do
    s <- gsapiEvalString $gshere sv
    throwIO (GSExcAbend pos s) :: IO GSValue

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

gsfile_write = $gsimpprim $ \ pos t fn s -> do
    fns <- gsapiEvalString pos fn
    let ?enc = UTF8Strict in withFile fns WriteMode $ \ h -> gsprimprint h pos t s

gsdir_read = $gsimpprim $ \ pos t fn -> do
    fns <- gsapiEvalString pos fn
    mbas <- try $ getDirectoryContents fns
    case mbas of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsdir_read " ++ show fns ++ " (getDirectoryContents returned Left (" ++ show e ++ ")) next"
        Right as -> $gslazylist =<< mapM $gslazystring (filter (\ a -> a /= "." && a /= "..") as)

gsprint :: GSValue
gsprint = $gsimpprim (gsprimprint stdout)

gsprintError :: GSValue
gsprintError = $gsimpprim (gsprimprint stderr)

gsprimprint :: Handle -> Pos -> Thread -> GSValue -> IO GSValue
gsprimprint h pos t (GSThunk th) = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h pos t v
gsprimprint h pos t (GSInvalidProgram err) = do
    hPutStrLn stderr $ fmtInvalidProgram err
    return $ $gsundefined_value
gsprimprint h pos t (GSImplementationFailure pos1 msg) = do
    hPutStrLn stderr $ fmtPos pos1 $ msg
    return $ $gsundefined_value
gsprimprint h pos t (GSError err) = do
    hPutStrLn stderr $ fmtError err
    return $ $gsundefined_value
gsprimprint h pos t (GSConstr pos1 c []) | c == gsvar "nil" =
    return $ $gsundefined_value
gsprimprint h pos t (GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = do
    let ?enc = UTF8Strict in hPutChar h ch
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c [ GSThunk th, s ]) | c == gsvar ":" = do
    v <- evalSync [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h pos t (GSConstr pos1 c [ v, s ])
gsprimprint h pos t (GSConstr pos1 c [ GSImplementationFailure pos2 msg, s ]) | c == gsvar ":" = do
    hPutStrLn stderr $ fmtPos pos2 $ msg
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c [ ch, s ]) | c == gsvar ":" = do
    hPutStrLn stderr $ fmtPos $gshere $ "gsprimprint (" ++ gsvCode ch ++ " : _) next"
    gsprimprint h pos t s
gsprimprint h pos t (GSConstr pos1 c as) =
    $apiImplementationFailure $ "gsprimprint " ++ fmtVarAtom c " next"
gsprimprint h pos t msg =
    $apiImplementationFailure $ "gsprimprint " ++ gsvCode msg ++ " next"

gsENOENT_view :: GSValue
gsENOENT_view = $gslambda_value $ \ ek -> $gsbcarg $ \ sk -> $gsbcarg $ \ err -> $gsbcconstr_view "ENOENT" ek sk err
