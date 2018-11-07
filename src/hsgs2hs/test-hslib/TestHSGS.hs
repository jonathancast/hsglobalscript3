{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr, printTestValue) where

import qualified Data.Map as Map

import GSI.Util (StackTrace(..), gshere, fmtPos)
import GSI.Syn (GSVar, gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.Error (fmtErrorShort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr, gsthunk, gsvCode, bcoCode)
import GSI.Eval (evalSync)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = formatTestValue v (putStrLn . ($ ""))

formatTestValue :: GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValue v@GSImplementationFailure{} k = formatTestValueAtom v k
formatTestValue v@GSError{} k = formatTestValueAtom v k
formatTestValue (GSThunk ts) k = do
    v <- evalSync [StackTrace $gshere []] ts
    formatTestValue v k
formatTestValue (GSClosure _ GSLambda{}) k = k $ ("<function>"++)
formatTestValue (GSClosure _ bco) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue (GSClosure _ "++) . (bcoCode bco++) . (") next>"++)
formatTestValue v@GSRecord{} k = formatTestValueAtom v k
formatTestValue v@GSNatural{} k = formatTestValueAtom v k
formatTestValue v@(GSConstr pos c [ GSThunk chts, s ]) k | c == gsvar ":" = do
    chv <- evalSync [StackTrace $gshere []] chts
    formatTestValue (GSConstr pos (gsvar ":") [ chv, s ]) k
formatTestValue v@(GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" = formatTestValueAtom v k
formatTestValue (GSConstr _ v as) k = formatArgs as $ \ ds -> k (fmtVarAtom v . ds)
formatTestValue v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue "++) . (gsvCode v++) . (" next>"++)

formatTestValueAtom :: GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValueAtom (GSImplementationFailure pos msg) k = k $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msg++) . ('>':)
formatTestValueAtom (GSError err) k = k $ ('<':) . (fmtErrorShort err++) . ('>':)
formatTestValueAtom (GSThunk ts) k = do
    v <- evalSync [StackTrace $gshere []] ts
    formatTestValueAtom v k
formatTestValueAtom (GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" =
    formatChar (GSRune ch) $ \ chds -> formatString s $ \ sds -> k (("qq{"++) . chds . sds . ('}':))
formatTestValueAtom (GSConstr pos c []) k = k $ fmtVarAtom c
formatTestValueAtom v@GSConstr{} k = formatTestValue v $ \ ds -> k $ ('(':) . ds . (')':)
formatTestValueAtom (GSRecord _ fs) k = case Map.null fs of
    True -> k $ ("〈〉"++)
    False -> formatFields (Map.assocs fs) $ \ ds -> k $ ('〈':) . (' ':) . ds . ('〉':)
formatTestValueAtom (GSNatural n) k = k $ shows n
formatTestValueAtom (GSRune r) k
    | r `elem` "/\\§()[]{}\n\t" = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom (GSRune "++) . shows r . (") next>"++)
    | otherwise = k $ ("r/"++) . (r:) . ('/':)
formatTestValueAtom v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom "++) . (gsvCode v++) . (" next>"++)

formatArgs :: [GSValue] -> ((String -> String) -> IO a) -> IO a
formatArgs (x:xn) k = formatTestValueAtom x $ \ xds -> formatArgs xn $ \ xnds -> k $ (' ':) . xds . xnds
formatArgs [] k = k id

formatFields :: [(GSVar, GSValue)] -> ((String -> String) -> IO a) -> IO a
formatFields ((v, x):fs) k = formatTestValue x $ \ xds -> formatFields fs $ \ fsds ->
    k $ fmtVarBindAtom v . (" = "++) . xds . ("; "++) . fsds
formatFields [] k = k id

formatChar :: GSValue -> ((String -> String) -> IO a) -> IO a
formatChar (GSRune '\n') k = k $ ("\\n"++)
formatChar (GSRune ch) k = k $ (ch:)
formatChar v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatChar "++) . (gsvCode v++) . (" next>"++)

formatString :: GSValue -> ((String -> String) -> IO a) -> IO a
formatString (GSThunk ts) k = do
    v <- evalSync [StackTrace $gshere []] ts
    formatString v k
formatString (GSConstr _ c [ r0, s1 ]) k | c == gsvar ":" = formatChar r0 $ \ r0ds -> formatString s1 $ \ s1ds -> k $ r0ds . s1ds
formatString (GSConstr _ c []) k | c == gsvar "nil" = k $ id
formatString (GSConstr _ c as) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . fmtVarAtom c . (" next>"++)
formatString v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . (gsvCode v++) . (" next>"++)
