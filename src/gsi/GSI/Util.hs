{-# LANGUAGE TemplateHaskell #-}
module GSI.Util (Pos(Pos), StackTrace(..), gshere, gsfatal, fmtPos, fmtStackTrace) where

import Language.Haskell.TH.Syntax (Lit(IntegerL), Loc, location, loc_filename, loc_start)
import Language.Haskell.TH.Lib (ExpQ, appE, conE, litE, stringE, varE)

data Pos = Pos {
    filename :: String,
    line :: Integer,
    col :: Integer
  }
  deriving (Eq, Show)

data StackTrace = StackTrace Pos [StackTrace]
  deriving (Show)

gshere :: ExpQ
gshere = do
    l <- location
    let (ln, col) = loc_start l
    conE 'Pos `appE` (stringE $ loc_filename l) `appE` (litE $ IntegerL $ toInteger $ ln) `appE` (litE $ IntegerL $ toInteger $ col)

gsfatal :: ExpQ
gsfatal = varE 'gsfatal_w `appE` gshere

gsfatal_w :: Pos -> String -> a
gsfatal_w pos msg = error $ fmtPos pos $ msg

fmtPos :: Pos -> String -> String
fmtPos p s = filename p ++ ':' : show (line p) ++ ':' : show (col p) ++ ": " ++ s

fmtStackTrace :: StackTrace -> String -> String
fmtStackTrace (StackTrace pos cs) msg = fmtPos pos $ msg ++ fmtCallers 0 cs "" where
    fmtCallers n [] s = s
    fmtCallers n [c] s = '\n' : replicate (n * 4) ' ' ++ fmtStackTrace' n c s
    fmtCallers n (c0:cs) s = '\n' : replicate ((n+1) * 4) ' ' ++ "(called from " ++ fmtStackTrace' (n + 1) c0 (')' : fmtCallers n cs s)

    fmtStackTrace' n (StackTrace pos cs) s = fmtPos' pos (fmtCallers n cs s)

fmtPos' :: Pos -> String -> String
fmtPos' p s = filename p ++ ':' : show (line p) ++ ':' : show (col p) ++ s
