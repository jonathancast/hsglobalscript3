{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Env (runGSProgram, gsabend, gsfile_stat, gsfile_read, gsfile_write, gsdir_read, gsprint, gsprintError, gsenv_var_get, gssystem, gsENOENT_view) where

import qualified Data.Map as Map

import Control.Exception (SomeException, try, catch, throwIO, fromException, displayException)

import System.IO (Handle, IOMode(..), withFile, hPutStrLn, hPutChar, stdout, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..), exitWith)

import System.Posix.Files (getFileStatus, isDirectory, modificationTime)

import System.Process (withCreateProcess, proc, waitForProcess)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (GSException(..), fmtInvalidProgram, fmtError)
import GSI.Value (GSValue(..), gslambda_value, gsapply, gsimpprim, gsundefined_value, gsvCode)
import GSI.Functions (gsbool, gslist, gsstring)
import GSI.ByteCode (gsbcarg, gsbcconstr_view)
import GSI.ThreadType (Thread)
import GSI.Thread (createThread, execMainThread)
import GSI.Eval (evalSync)
import API (apiImplementationFailure)
import GSI.Functions (gslazylist, gslazystring, gsapiEvalList, gsapiEvalString)

runGSProgram a = do
    as <- $gslist . map $gsstring <$> getArgs
    prog <- $gsapply a [as]
    t <- createThread $gshere prog Nothing
    execMainThread t
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

gsabend = $gsimpprim $ \ pos t  sv -> do
    s <- gsapiEvalString $gshere sv
    throwIO (GSExcAbend pos s) :: IO GSValue

gsfile_stat :: GSValue
gsfile_stat = $gsimpprim $ \ pos t fn -> do
    fns <- gsapiEvalString pos fn
    mbst <- try $ getFileStatus fns
    case mbst of
        Left e | Just e1 <- fromException e, isDoesNotExistError e1 ->
            return $ GSConstr $gshere (gsvar "left") [ GSConstr $gshere (gsvar "ENOENT") [ fn ] ]
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimenvFileStat " ++ show fns ++ " (stat returned Left (" ++ show e ++ ")) next"
        Right st -> return $ GSConstr $gshere (gsvar "right") [ GSRecord $gshere $ Map.fromList [
            (gsvar "is.dir", gsbool $ isDirectory st),
            (gsvar "mod.time", GSRational $ toRational $ modificationTime st)
          ] ]

gsfile_read :: GSValue
gsfile_read = $gsimpprim $ \ pos t fn -> do
    fns <- gsapiEvalString pos fn
    mbs <- try $ readFile fns
    case mbs of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimfileRead " ++ show fns ++ " (readFile returned Left (" ++ show e ++ ")) next"
        Right s -> $gslazystring s

gsfile_write = $gsimpprim $ \ pos t fn s -> do
    fns <- gsapiEvalString pos fn
    withFile fns WriteMode $ \ h -> gsprimprint h pos t s

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
    hPutChar h ch
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

gsenv_var_get = $gsimpprim $ \ pos t nm -> do
    nmhs <- gsapiEvalString pos nm
    mb <- try $ getEnv nmhs
    case mb of
        Right valhs -> return $ $gsstring valhs
        Left (e::SomeException) -> $apiImplementationFailure $ "gsenv_var_get: getEnv threw " ++ displayException e ++ " next"

gssystem = $gsimpprim $ \ pos t args -> do
    argshs0 <- gsapiEvalList pos args
    argshs <- mapM (gsapiEvalString pos) argshs0
    case argshs of
        cmd:argshs' -> do
            mb <- try $ withCreateProcess (proc cmd argshs') $ \ _ _ _ ph -> waitForProcess ph
            case mb of
                Right ExitSuccess   -> return $ $gsundefined_value
                Right (ExitFailure r) -> throwIO (GSExcAbend pos $ cmd ++ ": exit " ++ show r) :: IO GSValue
                Left (e::SomeException) -> $apiImplementationFailure $ "gssystem: withCreateProcess / waitForProcess threw " ++ displayException e ++ " next"
        _ -> $apiImplementationFailure $ "gssystem " ++ show argshs ++ " next"

gsENOENT_view = $gsbcconstr_view "ENOENT"
