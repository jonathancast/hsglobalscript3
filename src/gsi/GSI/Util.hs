{-# LANGUAGE TemplateHaskell #-}
module GSI.Util (Pos(Pos), gsfmtLocation, fmtPos) where

import Language.Haskell.TH.Syntax (Lit(IntegerL), Loc, loc_filename, loc_start)
import Language.Haskell.TH.Lib (ExpQ, appE, conE, litE, stringE, )

data Pos = Pos {
    filename :: String,
    line :: Integer
  }
  deriving (Eq, Show)

gsfmtLocation :: Loc -> ExpQ
gsfmtLocation l = conE 'Pos `appE` (stringE $ loc_filename l) `appE` (litE $ IntegerL $ toInteger $ ln) where
    (ln, col) = loc_start l

fmtPos :: Pos -> String -> String
fmtPos p s = filename p ++ ':' : show (line p) ++ ": " ++ s
