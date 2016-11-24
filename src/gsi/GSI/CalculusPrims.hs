{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gspriminsufficientcases) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), gsimplementationFailure, gsvCode)
import GSI.Eval (evalSync)

gspriminsufficientcases :: Pos -> GSValue -> IO GSValue
gspriminsufficientcases pos v@GSError{} = return v
gspriminsufficientcases pos v@GSImplementationFailure{} = return v
gspriminsufficientcases pos (GSThunk th) = do
    v <- evalSync th
    gspriminsufficientcases pos v
gspriminsufficientcases pos e = return $ $gsimplementationFailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"
