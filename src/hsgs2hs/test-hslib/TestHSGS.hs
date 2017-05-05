{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr) where

import GSI.Value (GSValue, GSExpr, gsthunk, gsvCode)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = formatTestValue v putStrLn

formatTestValue :: GSValue -> (String -> IO a) -> IO a
formatTestValue v k = k $ "<unimpl: formatTestValue " ++ gsvCode v ++ " next>"
