{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module GSI.ByteCode (GSBCO, ToGSBCO(..)) where

data GSBCO

class ToGSBCO a where
    gsbco :: a -> GSBCO
