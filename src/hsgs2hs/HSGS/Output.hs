{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode) where

import Data.Set (Set)

import GSI.Util (Pos, gsfatal)

data DestComp
  = DCChar Char
  | DCPos Pos
  | DCImports (Set HSImport)
  | DCExpr (Set HSImport) HSExpr

data HSImport
  = HSIType String String
  | HSITypeName String String
  | HSIVar String String
  deriving (Eq, Ord)

data HSExpr
  = HSConstr String
  | HSVar String
  | HSChar Char
  | HSString String
  | HSInteger Integer
  | HSList [HSExpr]
  | HSApp HSExpr HSExpr
  | HSLambda [String] HSExpr
  | HSAsType HSExpr String

dcCode :: DestComp -> String
dcCode DCChar{} = "DCChar"
dcCode DCPos{} = "DCPos"
dcCode DCImports{} = "DCImports"
dcCode DCExpr{} = "DCExpr"

hsiCode :: HSImport -> String
hsiCode HSIType{} = "HSIType"
hsiCode HSITypeName{} = "HSITypeName"
hsiCode HSIVar{} = "HSIVar"

hsCode :: HSExpr -> String
hsCode HSConstr{} = "HSConstr"
hsCode HSVar{} = "HSVar"
hsCode HSChar{} = "HSChar"
hsCode HSString{} = "HSString"
hsCode HSInteger{} = "HSInteger"
hsCode HSList{} = "HSList"
hsCode HSApp{} = "HSApp"
hsCode HSLambda{} = "HSLambda"
hsCode HSAsType{} = "HSAsType"
