{-# LANGUAGE TemplateHaskell #-}
module GSI.Util (Pos(Pos), StackTrace(..), compilationTime, gshere, gsfatal, fmtPos, fmtStackTrace, filename, line, col) where

import Language.Haskell.TH.Syntax (Lit(IntegerL), Loc, lift, runIO, location, loc_filename, loc_start)
import Language.Haskell.TH.Lib (ExpQ, appE, conE, litE, stringE, varE)

import Data.Time.Calendar (Day(..))
import Data.Time.Clock (UTCTime(..), getCurrentTime)

data Pos = Pos {
    filename :: String,
    line :: Integer,
    col :: Integer
  }
  deriving (Eq, Show)

data StackTrace = StackTrace Pos [StackTrace]
  deriving (Show)

compilationTime :: ExpQ
compilationTime = do
    UTCTime d t <- runIO getCurrentTime
    conE 'UTCTime `appE` (conE 'ModifiedJulianDay `appE` (lift $ toModifiedJulianDay d)) `appE` (varE 'fromRational `appE` (lift $ toRational t))

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
fmtStackTrace (StackTrace pos cs) msg = fmtPos pos $ msg ++ fmtCallers 10 0 cs "" where
    fmtCallers 0 _ _ s = "\n..." ++ s
    fmtCallers d n [] s = s
    fmtCallers d n [c] s = '\n' : replicate (n * 4) ' ' ++ fmtStackTrace' d n c s
    fmtCallers d n (c0:cs) s = '\n' : replicate ((n+1) * 4) ' ' ++ "(called from " ++ fmtStackTrace' (d-1) (n + 1) c0 (')' : fmtCallers (d -1) n cs s)

    fmtStackTrace' d n (StackTrace pos cs) s = fmtPos' pos (fmtCallers (d-1) n cs s)

fmtPos' :: Pos -> String -> String
fmtPos' p s = filename p ++ ':' : show (line p) ++ ':' : show (col p) ++ s
