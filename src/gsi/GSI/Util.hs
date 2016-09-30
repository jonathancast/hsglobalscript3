{-# LANGUAGE TemplateHaskell #-}
module GSI.Util (Pos(Pos), gshere, gsfatal, fmtPos) where

import Language.Haskell.TH.Syntax (Lit(IntegerL), Loc, location, loc_filename, loc_start)
import Language.Haskell.TH.Lib (ExpQ, appE, conE, litE, stringE, varE)

data Pos = Pos {
    filename :: String,
    line :: Integer
  }
  deriving (Eq, Show)

gshere :: ExpQ
gshere = do
    l <- location
    let (ln, col) = loc_start l
    conE 'Pos `appE` (stringE $ loc_filename l) `appE` (litE $ IntegerL $ toInteger $ ln)

gsfatal :: ExpQ
gsfatal = varE 'gsfatal_w `appE` gshere

gsfatal_w :: Pos -> String -> a
gsfatal_w pos msg = error $ fmtPos pos $ msg

fmtPos :: Pos -> String -> String
fmtPos p s = filename p ++ ':' : show (line p) ++ ": " ++ s
