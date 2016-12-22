{-# OPTIONS_GHC -fwarn-incomplete-patterns -fno-warn-overlapping-patterns #-}
module GSI.Syn (GSVar, gsvar, fmtVarAtom) where -- §hs{GSVar} is deliberately §emph{abstract}

data GSVar = GSVar String

gsvar :: String -> GSVar
gsvar v = GSVar v

fmtVarAtom :: GSVar -> String -> String
fmtVarAtom v = ("<Unknown var type "++) . (gsvarCode v++) . ('>':)

gsvarCode :: GSVar -> String
gsvarCode GSVar{} = "GSVar"
