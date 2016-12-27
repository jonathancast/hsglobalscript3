{-# LANGUAGE TemplateHaskell #-}
module GSI.Prims (gsparand) where

import GSI.Util (Pos)
import GSI.Value (GSValue, gsimplementationFailure, gsvCode)

gsparand :: Pos -> GSValue -> GSValue -> IO GSValue
gsparand pos x y = return $ $gsimplementationFailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"
