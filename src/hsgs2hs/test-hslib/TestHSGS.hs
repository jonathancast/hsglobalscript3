{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr) where

import qualified Data.Map as Map

import GSI.Util (StackTrace(..), gshere, fmtPos)
import GSI.Value (GSValue(..), GSExpr, gsthunk, gsvCode)
import GSI.Eval (evalSync)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = formatTestValue v (putStrLn . ($ ""))

formatTestValue :: GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValue v@GSImplementationFailure{} k = formatTestValueAtom v k
formatTestValue (GSThunk ts) k = do
    v <- evalSync [StackTrace $gshere []] ts
    formatTestValue v k
formatTestValue v@GSRecord{} k = formatTestValueAtom v k
formatTestValue v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue "++) . (gsvCode v++) . (" next>"++)

formatTestValueAtom :: GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValueAtom (GSImplementationFailure pos msg) k = k $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msg++) . ('>':)
formatTestValueAtom (GSRecord _ fs) k = case Map.null fs of
    True -> k $ ("〈〉"++)
    False -> k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom (GSRecord _ fs) next>"++)
formatTestValueAtom v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom "++) . (gsvCode v++) . (" next>"++)