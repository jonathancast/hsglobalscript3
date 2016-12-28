{-# LANGUAGE TemplateHaskell #-}
module GSI.Prims (gsparand) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), gsimplementationFailure, gsvCode)
import GSI.Eval (evalSync)

gsparand :: Pos -> GSValue -> GSValue -> IO GSValue
-- > gsparand pos fail _ = fail
-- > gsparand pos _ fail = fail
-- > gsparand pos succeed succeed = succeed
-- > gsparand pos thunk thunk = eval both, wait for one, then loop
gsparand pos x@GSImplementationFailure{} y@GSImplementationFailure{} = return x
gsparand pos (GSThunk xs) y@GSImplementationFailure{} = do
    xv <- evalSync xs
    gsparand pos xv y
gsparand pos x y = return $ $gsimplementationFailure $ "gsparand " ++ gsvCode x ++ ' ' : gsvCode y ++ " next"
