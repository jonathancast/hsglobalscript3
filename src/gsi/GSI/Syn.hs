{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Syn (GSVar, gsvar, fmtVarAtom) where -- §hs{GSVar} is deliberately §emph{abstract}

import Data.Char (isPunctuation, isSymbol)

import GSI.Util (gsfatal)

data GSVar = GSVarSym String
    deriving (Eq)

gsvar :: String -> GSVar
gsvar [] = $gsfatal "gsvar [] called; this is very bad"
gsvar v@(c:_) =
  if isPunctuation c || isSymbol c then GSVarSym v
  else $gsfatal $ "gsvar " ++ show v ++ " next"

fmtVarAtom :: GSVar -> String -> String
fmtVarAtom (GSVarSym o) = ('(':) . (o++) . (')':)
fmtVarAtom v = ("<Unknown var type "++) . (gsvarCode v++) . ('>':)

gsvarCode :: GSVar -> String
gsvarCode GSVarSym{} = "GSVarSym"
