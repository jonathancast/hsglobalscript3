{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Syn (GSVar, gsvar, fmtVarAtom) where -- §hs{GSVar} is deliberately §emph{abstract}

import Data.Char (isPunctuation, isSymbol, isDigit)

import GSI.Util (gsfatal)

data GSVar
  = GSVarSym String
  | GSVarNum Integer
    deriving (Eq, Ord)

gsvar :: String -> GSVar
gsvar [] = $gsfatal "gsvar [] called; this is very bad"
gsvar v@(c:_) =
  if isPunctuation c || isSymbol c then GSVarSym v
  else if isDigit c then GSVarNum (read v)
  else $gsfatal $ "gsvar " ++ show v ++ " next"

fmtVarAtom :: GSVar -> String -> String
fmtVarAtom (GSVarSym o) = ('(':) . (o++) . (')':)
fmtVarAtom (GSVarNum n) = shows n
fmtVarAtom v = ("<Unknown var type "++) . (gsvarCode v++) . ('>':)

gsvarCode :: GSVar -> String
gsvarCode GSVarSym{} = "GSVarSym"
gsvarCode GSVarNum{} = "GSVarNum"
