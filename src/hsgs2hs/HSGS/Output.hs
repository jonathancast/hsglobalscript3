{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode) where

import Data.Set (Set)

import GSI.Util (gsfatal)

data DestComp
  = DCChar Char
  | DCImports (Set HSImport)
  | DCExpr (Set HSImport) HSExpr

data HSImport
  = HSIType String String
  deriving (Eq, Ord)

data HSExpr
  = HSConstr String
  | HSVar String
  | HSApp HSExpr HSExpr

dcCode :: DestComp -> String
dcCode DCChar{} = "DCChar"
dcCode DCImports{} = "DCImports"
dcCode DCExpr{} = "DCExpr"

hsiCode :: HSImport -> String
hsiCode i = i `seq` $gsfatal "hsiCode next"

hsCode :: HSExpr -> String
hsCode HSConstr{} = "HSConstr"
hsCode HSVar{} = "HSVar"
hsCode HSApp{} = "HSApp"
