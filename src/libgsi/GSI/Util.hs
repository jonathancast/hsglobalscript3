{-# LANGUAGE TemplateHaskell #-}
module GSI.Util (Pos(Pos), StackTrace(..), compilationTime, gshere, gsfatal, fmtPos, fmtStackTrace, fmtCallers, filename, line, col) where

import Language.Haskell.TH.Syntax (Lit(IntegerL), lift, runIO, location, loc_filename, loc_start)
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
  deriving (Eq, Show)

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
fmtStackTrace (StackTrace pos cs) msg = fmtPos pos $ msg ++ fmtCallers cs "" where

fmtCallers :: [StackTrace] -> String -> String
fmtCallers cs s = fmtCallers' 0 (prune 10 cs) s where
    fmtCallers' n (Callers []) = id
    fmtCallers' n Pruned = ('\n':) . (replicate (n * 4) ' '++) . ("..."++)
    fmtCallers' n (Callers [(pos, c)]) = ('\n':) . (replicate (n * 4) ' '++) . fmtPos' pos . fmtCallers' n c
    fmtCallers' n (Callers ((pos, c0):cs)) = ('\n':) . (replicate ((n+1) * 4) ' '++) . ("(called from "++) . fmtPos' pos . fmtCallers' (n+1) c0 . (replicate (n * 4) ' '++) . (')':) . fmtCallers' n (Callers cs)

prune n [] = Callers []
prune 0 _ = Pruned
prune d cs = Callers $ concatMap process cs where
    process (StackTrace pos cs') = return (pos, prune (d-1) cs')

data PrunedTree
  = Callers [(Pos, PrunedTree)]
  | Pruned

fmtPos' :: Pos -> String -> String
fmtPos' p s = filename p ++ ':' : show (line p) ++ ':' : show (col p) ++ s
