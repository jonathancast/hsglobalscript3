{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Env (runGSProgram, gsabend, gsfile_stat, gsfile_read, gsfile_write, gsdir_read, gsprint, gsprintError, gsenv_var_get, gssystem, gsENOENT_view) where

import qualified Data.Map as Map

import Control.Monad (forever)

import Control.Concurrent (forkIO)
import Control.Exception (SomeException, try, catch, finally, throwIO, fromException, displayException)

import System.IO (Handle, IOMode(..), openFile, hClose, withFile, hPutStrLn, hPutChar, stdout, stderr)
import System.IO.Error (isDoesNotExistError)
import System.Directory (getDirectoryContents)
import System.Environment (getArgs, getEnv)
import System.Exit (ExitCode(..), exitWith)

import System.Posix.Files (getFileStatus, isDirectory, modificationTime)

import System.Process (withCreateProcess, proc, waitForProcess)

import GSI.Util (Pos, StackTrace(..), gshere, fmtPos, fmtStackTrace)
import GSI.Syn (gsvar, fmtVarAtom)
import GSI.Error (GSException(..), fmtInvalidProgram, fmtError)
import GSI.Message (Message(..), msgCode)
import GSI.Prof (ProfCounter, newProfCounter)
import GSI.RTS (OPort, newEvent, wakeup, await, awaitAny, newChannel, iportReadable, tryReadIPort)
import GSI.Value (GSValue(..), GSEvalState(..), gslambda_value, gsapply, gsimpprim, gsundefined_value, gsvCode)
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
    gsprof <- getEnv "GSPROF" `catch` \ e -> if isDoesNotExistError e then return "" else throwIO e
    pc <- getProfCounter gsprof
    logCatcher <- getLogCatcher gsprof
    mainDone <- newEvent
    msgDone <- newEvent
    (msgi, msgo) <- newChannel
    forkIO $ logCatcher msgi mainDone msgDone
    t <- createThread msgo pc $gshere prog Nothing
    execMainThread t
        `finally` (wakeup mainDone *> await msgDone)
  `catch` \ e -> hPutStrLn stderr (displayException (e :: SomeException)) >> exitWith (ExitFailure 1) -- Because Haskell is a conspiracy to avoid good error messages

getProfCounter "" = return Nothing
getProfCounter gsprof = Just <$> newProfCounter

getLogCatcher gsprof = do
    mbh <- case gsprof of
        "" -> return Nothing
        _ -> Just <$> openFile (gsprof ++ ".prof") AppendMode
    return $ stderrLogCatcher mbh

stderrLogCatcher mbprof msg mainDone done = do
    msgReady <- iportReadable msg
    awaitAny [ msgReady, mainDone ]
    mbm <- tryReadIPort msg
    case mbm of
        Nothing -> maybe (return ()) hClose mbprof *> wakeup done
        Just m -> do
            stderrLog mbprof m  `catch` \ (e :: SomeException) -> hPutStrLn stderr (displayException e)
            stderrLogCatcher mbprof msg mainDone done

stderrLog (Just h) (MsgProfile st) = hPutStrLn h $ fmtStackTrace st "Prof"
stderrLog _ m = hPutStrLn stderr $ "Unknown log message " ++ msgCode m

gsabend = $gsimpprim $ \ evs pos t  sv -> do
    s <- gsapiEvalString (msgChannel evs) (profCounter evs) $gshere sv
    throwIO (GSExcAbend pos s) :: IO GSValue

gsfile_stat :: GSValue
gsfile_stat = $gsimpprim $ \ evs pos t fn -> do
    fns <- gsapiEvalString (msgChannel evs) (profCounter evs) pos fn
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
gsfile_read = $gsimpprim $ \ evs pos t fn -> do
    fns <- gsapiEvalString (msgChannel evs) (profCounter evs) pos fn
    mbs <- try $ readFile fns
    case mbs of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsprimfileRead " ++ show fns ++ " (readFile returned Left (" ++ show e ++ ")) next"
        Right s -> $gslazystring s

gsfile_write = $gsimpprim $ \ evs pos t fn s -> do
    fns <- gsapiEvalString (msgChannel evs) (profCounter evs) pos fn
    withFile fns WriteMode $ \ h -> gsprimprint h evs pos t s

gsdir_read = $gsimpprim $ \ evs pos t fn -> do
    fns <- gsapiEvalString (msgChannel evs) (profCounter evs) pos fn
    mbas <- try $ getDirectoryContents fns
    case mbas of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsdir_read " ++ show fns ++ " (getDirectoryContents returned Left (" ++ show e ++ ")) next"
        Right as -> $gslazylist =<< mapM $gslazystring (filter (\ a -> a /= "." && a /= "..") as)

gsprint :: GSValue
gsprint = $gsimpprim (gsprimprint stdout)

gsprintError :: GSValue
gsprintError = $gsimpprim (gsprimprint stderr)

gsprimprint :: Handle -> GSEvalState -> Pos -> Thread -> GSValue -> IO GSValue
gsprimprint h evs pos t (GSThunk th) = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h evs pos t v
gsprimprint h evs pos t (GSInvalidProgram err) = do
    hPutStrLn stderr $ fmtInvalidProgram err
    return $ $gsundefined_value
gsprimprint h evs pos t (GSImplementationFailure pos1 msgs) = do
    hPutStrLn stderr $ fmtPos pos1 $ msgs
    return $ $gsundefined_value
gsprimprint h evs pos t (GSError err) = do
    hPutStrLn stderr $ fmtError err
    return $ $gsundefined_value
gsprimprint h evs pos t (GSConstr pos1 c []) | c == gsvar "nil" =
    return $ $gsundefined_value
gsprimprint h evs pos t (GSConstr pos1 c [ GSRune ch, s ]) | c == gsvar ":" = do
    hPutChar h ch
    gsprimprint h evs pos t s
gsprimprint h evs pos t (GSConstr pos1 c [ GSThunk th, s ]) | c == gsvar ":" = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere [StackTrace pos []]] th
    gsprimprint h evs pos t (GSConstr pos1 c [ v, s ])
gsprimprint h evs pos t (GSConstr pos1 c [ GSImplementationFailure pos2 msgs, s ]) | c == gsvar ":" = do
    hPutStrLn stderr $ fmtPos pos2 $ msgs
    gsprimprint h evs pos t s
gsprimprint h evs pos t (GSConstr pos1 c [ ch, s ]) | c == gsvar ":" = do
    hPutStrLn stderr $ fmtPos $gshere $ "gsprimprint (" ++ gsvCode ch ++ " : _) next"
    gsprimprint h evs pos t s
gsprimprint h evs pos t (GSConstr pos1 c as) =
    $apiImplementationFailure $ "gsprimprint " ++ fmtVarAtom c " next"
gsprimprint h evs pos t msgv =
    $apiImplementationFailure $ "gsprimprint " ++ gsvCode msgv ++ " next"

gsenv_var_get = $gsimpprim $ \ evs pos t nm -> do
    nmhs <- gsapiEvalString (msgChannel evs) (profCounter evs) pos nm
    mb <- try $ getEnv nmhs
    case mb of
        Right valhs -> return $ $gsstring valhs
        Left (e::SomeException) -> $apiImplementationFailure $ "gsenv_var_get: getEnv threw " ++ displayException e ++ " next"

gssystem = $gsimpprim $ \ evs pos t args -> do
    argshs0 <- gsapiEvalList (msgChannel evs) (profCounter evs) pos args
    argshs <- mapM (gsapiEvalString (msgChannel evs) (profCounter evs) pos) argshs0
    case argshs of
        cmd:argshs' -> do
            mb <- try $ withCreateProcess (proc cmd argshs') $ \ _ _ _ ph -> waitForProcess ph
            case mb of
                Right ExitSuccess   -> return $ $gsundefined_value
                Right (ExitFailure r) -> throwIO (GSExcAbend pos $ cmd ++ ": exit " ++ show r) :: IO GSValue
                Left (e::SomeException) -> $apiImplementationFailure $ "gssystem: withCreateProcess / waitForProcess threw " ++ displayException e ++ " next"
        _ -> $apiImplementationFailure $ "gssystem " ++ show argshs ++ " next"

gsENOENT_view = $gsbcconstr_view "ENOENT"
