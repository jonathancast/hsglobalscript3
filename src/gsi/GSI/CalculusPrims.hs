{-# LANGUAGE TemplateHaskell #-}
module GSI.CalculusPrims (gspriminsufficientcases) where

import GSI.Util (Pos)
import GSI.Value (GSValue, gsimplementationFailure, gsvCode)

gspriminsufficientcases :: Pos -> GSValue -> IO GSValue
gspriminsufficientcases pos e = return $ $gsimplementationFailure $ "gspriminsufficientcases " ++ gsvCode e ++ " next"
