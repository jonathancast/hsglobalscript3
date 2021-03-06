{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Syn (GSVar, gsvar, varName, fmtVarAtom, fmtVarBindAtom) where -- §hs{GSVar} is deliberately §emph{abstract}

import Data.Char (isPunctuation, isSymbol, isDigit, isLetter, isUpper)

import GSI.Util (gsfatal, gshere, fmtPos)

data GSVar
  = GSVarSym String
  | GSVarNum Integer
  | GSVarAlphaNum String
    deriving (Eq, Ord, Show)

gsvar :: String -> GSVar
gsvar [] = $gsfatal "gsvar [] called; this is very bad"
gsvar v@(c:_) =
  if isPunctuation c || isSymbol c then GSVarSym v
  else if isDigit c then GSVarNum (read v)
  else if isLetter c && not (isUpper c) then GSVarAlphaNum v
  else if isUpper c then GSVarAlphaNum v
  else $gsfatal $ "gsvar " ++ show v ++ " next"

varName :: GSVar -> String
varName (GSVarSym o) = o
varName (GSVarNum n) = show n
varName (GSVarAlphaNum s) = s
varName v = ('<':) . fmtPos $gshere . ("Unknown var type "++) . (gsvarCode v++) . ('>':) $ ""

fmtVarAtom :: GSVar -> String -> String
fmtVarAtom (GSVarSym o) = ('(':) . (o++) . (')':)
fmtVarAtom (GSVarNum n) = shows n
fmtVarAtom (GSVarAlphaNum s) = (s++)
fmtVarAtom v = ('<':) . fmtPos $gshere . ("Unknown var type "++) . (gsvarCode v++) . ('>':)

fmtVarBindAtom :: GSVar -> String -> String
fmtVarBindAtom (GSVarAlphaNum s) = ('\'':) . (s++)
fmtVarBindAtom (GSVarSym o) = ('(':) . ('\'':) . (o++) . (')':)
fmtVarBindAtom (GSVarNum n) = ('\'':) . shows n
fmtVarBindAtom v = ('\'':) . ('<':) . fmtPos $gshere . ("Unknown var type "++) . (gsvarCode v++) . ('>':)

gsvarCode :: GSVar -> String
gsvarCode GSVarSym{} = "GSVarSym"
gsvarCode GSVarNum{} = "GSVarNum"
gsvarCode GSVarAlphaNum{} = "GSVarAlphaNum"
