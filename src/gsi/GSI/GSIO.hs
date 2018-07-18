{-# LANGUAGE TemplateHaskell, ImplicitParams, ScopedTypeVariables #-}
module GSI.GSIO (gsio_monad, gsio_file_read) where

import Prelude hiding (readFile, writeFile) -- Because Haskell is stupid and evil

import qualified Data.Map as Map

import Control.Exception (SomeException, try)

import Data.Encoding.UTF8 (UTF8(..))
import System.IO.Encoding (readFile)

import GSI.Util (Pos, gshere)
import GSI.Syn (gsvar)
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

gsioprim_file_read :: Pos -> Thread -> GSValue -> IO GSValue
gsioprim_file_read pos t fn = do
    fns <- gsapiEvalString $gshere fn
    mbs <- try $ let ?enc = UTF8Strict in readFile fns
    case mbs of
        Left (e :: SomeException) -> $apiImplementationFailure $ "gsioprim_file_read " ++ show fns ++ " (readFile returned Left (" ++ show e ++ ")) next"
        Right s -> do
            sv <- $gslazystring s
            return $ GSConstr $gshere (gsvar "right") [ sv ]
