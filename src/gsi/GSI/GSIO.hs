{-# LANGUAGE TemplateHaskell #-}
module GSI.GSIO (gsio_monad) where

import qualified Data.Map as Map

import GSI.Util (gshere)
import GSI.Syn (gsvar)
import GSI.Value (GSValue(..), gslambda_value, gsav, gsae)
import GSI.ByteCode (gsbcarg, gsbcapply, gsbcimpfor, gsbcimpbind, gsbcimpbody, gsbcimpunit)

gsio_monad = GSRecord $gshere $ Map.fromList [
    (gsvar ">>=", $gslambda_value $ \ a -> $gsbcarg $ \ f -> $gsbcimpfor $ do
        x <- $gsbcimpbind $ $gsav a
        $gsbcimpbody $ $gsae $ $gsbcapply f [ $gsav x ]
    ),
    (gsvar "unit", $gslambda_value $ \ x -> $gsbcimpfor $ $gsbcimpunit $ $gsav x)
  ]
