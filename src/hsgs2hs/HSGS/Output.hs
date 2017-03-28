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
  | HSIVar String String
  deriving (Eq, Ord)

data HSExpr
  = HSConstr String
  | HSVar String
  | HSString String
  | HSApp HSExpr HSExpr

dcCode :: DestComp -> String
dcCode DCChar{} = "DCChar"
dcCode DCImports{} = "DCImports"
dcCode DCExpr{} = "DCExpr"

hsiCode :: HSImport -> String
hsiCode HSIType{} = "HSIType"
hsiCode HSIVar{} = "HSIVar"

hsCode :: HSExpr -> String
hsCode HSConstr{} = "HSConstr"
hsCode HSVar{} = "HSVar"
hsCode HSString{} = "HSString"
hsCode HSApp{} = "HSApp"
