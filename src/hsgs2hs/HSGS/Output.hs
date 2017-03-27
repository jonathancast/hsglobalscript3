{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (HSExpr(..), hsCode) where

import GSI.Util (gsfatal)

data HSExpr
  = HSApp HSExpr HSExpr

hsCode :: HSExpr -> String
hsCode HSApp{} = "HSApp"
