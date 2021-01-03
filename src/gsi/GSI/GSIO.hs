{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
module GSI.GSIO (gsio_monad, gsio_file_read) where

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import qualified Data.Map as Map

import Control.Exception (SomeException, try)
import System.IO (openFile, IOMode(ReadMode), hSetEncoding, utf8, hGetContents)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
import GSI.Message (Message)
import GSI.RTS (OPort)
import GSI.ThreadType (Thread)
import GSI.Value (GSValue(..), gslambda_value, gsimpprim, gsav, gsae)
import API (apiImplementationFailure)
import GSI.Functions (gslazystring, gsapiEvalString)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit)

gsio_monad = GSRecord $gshere $ Map.fromList [
    (gsvar ">>=", $gslambda_value $ \ a -> $gsbcarg $ \ f -> $gsbcimpfor $ do
        x <- $gsbcimpbind $ $gsav a
        $gsbcimpbody $ $gsae $ $gsbcapply f [ $gsav x ]
    ),
    (gsvar "unit", $gslambda_value $ \ x -> $gsbcimpfor $ $gsbcimpunit $ $gsav x)
  ]

gsio_file_read :: GSValue
gsio_file_read = $gsimpprim gsioprim_file_read

gsioprim_file_read :: OPort Message -> Pos -> Thread -> GSValue -> IO GSValue
gsioprim_file_read msg pos t fn = do
    fns <- gsapiEvalString msg $gshere fn
    mbs <- try $ do
        ifh <- openFile fns ReadMode
        hSetEncoding ifh utf8
        hGetContents ifh
    case mbs of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsioprim_file_read " ++ show fns ++ " (readFile returned Left (" ++ show e ++ ")) next"
        Right s -> do
            sv <- $gslazystring s
            return $ GSConstr $gshere (gsvar "right") [ sv ]
