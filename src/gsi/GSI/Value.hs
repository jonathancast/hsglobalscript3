{-# LANGUAGE TemplateHaskell #-}
module GSI.Value (GSValue(..), gsundefined, gsvCode) where

import Language.Haskell.TH.Lib (appE, conE)

import GSI.Util (Pos, gshere)

data GSValue a
  = GSUndefined Pos

gsundefined = conE 'GSUndefined `appE` gshere

gsvCode :: GSValue a -> String
gsvCode GSUndefined{} = "GSUndefined"
