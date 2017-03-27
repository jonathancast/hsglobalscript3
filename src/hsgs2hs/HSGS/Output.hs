{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (DestComp(..), HSExpr(..), dcCode, hsCode) where

import GSI.Util (gsfatal)

data DestComp
  = DCChar Char
  | DCExpr HSExpr

data HSExpr
  = HSConstr String
  | HSVar String
  | HSApp HSExpr HSExpr

dcCode :: DestComp -> String
dcCode DCChar{} = "DCChar"
dcCode DCExpr{} = "DCExpr"

hsCode :: HSExpr -> String
hsCode HSConstr{} = "HSConstr"
hsCode HSVar{} = "HSVar"
hsCode HSApp{} = "HSApp"
