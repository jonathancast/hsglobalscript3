{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..)) where

data GSBCO a

class ToGSBCO r a where
    gsbco :: r -> GSBCO a
