{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Output (HSExpr, hsCode) where

import GSI.Util (gsfatal)

data HSExpr

hsCode :: HSExpr -> String
hsCode = $gsfatal "hsCode next"