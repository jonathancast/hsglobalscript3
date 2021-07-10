{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr, printTestValue, printStackTrace) where

import qualified Data.Map as Map

import GSI.Util (StackTrace(..), gshere, fmtPos, fmtCallers)
import GSI.Syn (GSVar, gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.RTS (bitBucketOPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr, GSEvalState(..), gsthunk, gsvCode, bcoCode)
import GSI.Eval (evalSync)
import GSI.Functions (fmtErrorShort, fmtInvalidProgram)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = bitBucketOPort >>= \ msg -> formatTestValue (GSEvalState msg Nothing) v (putStrLn . ($ ""))

formatTestValue :: GSEvalState -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValue evs v@GSImplementationFailure{} k = formatTestValueAtom evs v k
formatTestValue evs v@GSError{} k = formatTestValueAtom evs v k
formatTestValue evs v@GSInvalidProgram{} k = formatTestValueAtom evs v k
formatTestValue evs (GSThunk ts) k = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere []] ts
    formatTestValue evs v k
formatTestValue evs (GSClosure _ GSLambda{}) k = k $ ("<function>"++)
formatTestValue evs (GSClosure _ bco) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue (GSClosure _ "++) . (bcoCode bco++) . (") next>"++)
formatTestValue evs v@GSRecord{} k = formatTestValueAtom evs v k
formatTestValue evs v@GSNatural{} k = formatTestValueAtom evs v k
formatTestValue evs v@GSRune{} k = formatTestValueAtom evs v k
formatTestValue evs v@(GSConstr pos c [ GSThunk chts, s ]) k | c == gsvar ":" = do
    chv <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere []] chts
    formatTestValue evs (GSConstr pos (gsvar ":") [ chv, s ]) k
formatTestValue evs v@(GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" = formatTestValueAtom evs v k
formatTestValue evs (GSConstr _ v as) k = formatArgs evs as $ \ ds -> k (fmtVarAtom v . ds)
formatTestValue evs v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue "++) . (gsvCode v++) . (" next>"++)

formatTestValueAtom :: GSEvalState -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValueAtom evs (GSImplementationFailure pos msgs) k = k $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msgs++) . ('>':)
formatTestValueAtom evs (GSError err) k = do errs <- fmtErrorShort err; k $ ('<':) . (errs++) . ('>':)
formatTestValueAtom evs (GSInvalidProgram err) k = k $ ('<':) . (fmtInvalidProgram err++) . ('>':)
formatTestValueAtom evs (GSThunk ts) k = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere []] ts
    formatTestValueAtom evs v k
formatTestValueAtom evs (GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" =
    formatChar (GSRune ch) $ \ chds -> formatString evs s $ \ sds -> k (("qq{"++) . chds . sds . ('}':))
formatTestValueAtom evs (GSConstr pos c []) k = k $ fmtVarAtom c
formatTestValueAtom evs v@GSConstr{} k = formatTestValue evs v $ \ ds -> k $ ('(':) . ds . (')':)
formatTestValueAtom evs (GSRecord _ fs) k = case Map.null fs of
    True -> k $ ("〈〉"++)
    False -> formatFields evs (Map.assocs fs) $ \ ds -> k $ ('〈':) . (' ':) . ds . ('〉':)
formatTestValueAtom evs (GSNatural _ n) k = k $ shows n
formatTestValueAtom evs (GSRune r) k
    | r `elem` "/\\§()[]{}\n\t" = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom (GSRune "++) . shows r . (") next>"++)
    | otherwise = k $ ("r/"++) . (r:) . ('/':)
formatTestValueAtom evs v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom "++) . (gsvCode v++) . (" next>"++)

formatArgs :: GSEvalState -> [GSValue] -> ((String -> String) -> IO a) -> IO a
formatArgs evs (x:xn) k = formatTestValueAtom evs x $ \ xds -> formatArgs evs xn $ \ xnds -> k $ (' ':) . xds . xnds
formatArgs evs [] k = k id

formatFields :: GSEvalState -> [(GSVar, GSValue)] -> ((String -> String) -> IO a) -> IO a
formatFields evs ((v, x):fs) k = formatTestValue evs x $ \ xds -> formatFields evs fs $ \ fsds ->
    k $ fmtVarBindAtom v . (" = "++) . xds . ("; "++) . fsds
formatFields evs [] k = k id

formatChar :: GSValue -> ((String -> String) -> IO a) -> IO a
formatChar (GSRune '\n') k = k $ ("\\n"++)
formatChar (GSRune '\\') k = k $ ("\\\\"++)
formatChar (GSRune ch) k = k $ (ch:)
formatChar v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatChar "++) . (gsvCode v++) . (" next>"++)

formatString :: GSEvalState -> GSValue -> ((String -> String) -> IO a) -> IO a
formatString evs (GSThunk ts) k = do
    v <- evalSync (msgChannel evs) (profCounter evs) [StackTrace $gshere []] ts
    formatString evs v k
formatString evs (GSConstr _ c [ r0, s1 ]) k | c == gsvar ":" = formatChar r0 $ \ r0ds -> formatString evs s1 $ \ s1ds -> k $ r0ds . s1ds
formatString evs (GSConstr _ c []) k | c == gsvar "nil" = k $ id
formatString evs (GSConstr _ c as) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . fmtVarAtom c . (" next>"++)
formatString evs v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . (gsvCode v++) . (" next>"++)

printStackTrace :: GSValue -> IO ()
printStackTrace v = bitBucketOPort >>= \ msg -> w msg Nothing v
  where
    w msg pc (GSThunk ts) = do
        v <- evalSync msg pc [StackTrace $gshere []] ts
        w msg pc v
    w _ pc (GSImplementationFailure pos msg) = putStr $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msg++) . ('>':) $ "\n"
    w _ pc (GSClosure cs _) = putStr $ fmtCallers cs "\n"
    w _ pc gsv = error $ "printStackTrace " ++ gsvCode gsv ++ " next"
