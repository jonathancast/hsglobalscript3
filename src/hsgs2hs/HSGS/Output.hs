{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsCode) where

import Data.Set (Set)

import GSI.Util (gsfatal)

data DestComp
  = DCChar Char
  | DCImports (Set HSImport)
  | DCExpr HSExpr

data HSImport = HSImport
  deriving (Eq, Ord)

data HSExpr
  = HSConstr String
  | HSVar String
  | HSApp HSExpr HSExpr

dcCode :: DestComp -> String
dcCode DCChar{} = "DCChar"
dcCode DCImports{} = "DCImports"
dcCode DCExpr{} = "DCExpr"

hsCode :: HSExpr -> String
hsCode HSConstr{} = "HSConstr"
hsCode HSVar{} = "HSVar"
hsCode HSApp{} = "HSApp"
