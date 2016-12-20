module GSI.Syn (GSVar, gsvar) where -- §hs{GSVar} is deliberately §emph{abstract}

data GSVar = GSVar String

gsvar :: String -> GSVar
gsvar v = GSVar v
