{-# LANGUAGE TemplateHaskell #-}
module GSI.ST (gsstrun) where

import GSI.Util (Pos)
import GSI.Value (GSValue(..), gsprim, gsundefined_value, gsimplementationfailure, gsvCode)

gsstrun :: GSValue
gsstrun = $gsprim gsprim_st_run :: GSValue

gsprim_st_run :: Pos -> GSValue -> IO GSValue
gsprim_st_run pos a = do
    return $ $gsimplementationfailure $ "gsprim_st_run " ++ gsvCode a ++ " next"
