{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gspriminsufficientcases) where

import GSI.Util (Pos)
import GSI.Error (GSError(..))
import GSI.Value (GSValue(..), gsimplementationFailure, gsvCode)
import GSI.Eval (evalSync)

gspriminsufficientcases :: Pos -> GSValue -> IO GSValue
gspriminsufficientcases pos v@GSError{} = return v
gspriminsufficientcases pos v@GSImplementationFailure{} = return v
gspriminsufficientcases pos (GSThunk th) = do
    v <- evalSync th
    gspriminsufficientcases pos v
gspriminsufficientcases pos v@GSConstr{} = GSError . GSErrInsufficientCases pos . ($ "") <$> fmtValue v -- Buggy buggy should take imported-as names into account
gspriminsufficientcases pos e = return $ $gsimplementationFailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"

fmtValue :: GSValue -> IO (String -> String)
fmtValue v = return $ ("<Unknown value: " ++) . (gsvCode v ++) . ('>':)
