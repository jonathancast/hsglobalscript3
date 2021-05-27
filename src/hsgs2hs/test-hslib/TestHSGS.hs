{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr, printTestValue, printStackTrace) where

import Control.Monad (forever)

import qualified Data.Map as Map

import GSI.Util (StackTrace(..), gshere, fmtPos, fmtCallers)
import GSI.Syn (GSVar, gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.Error (fmtErrorShort, fmtInvalidProgram, fmtInvalidProgramShort)
import GSI.Message (Message)
import GSI.Prof (ProfCounter)
import GSI.RTS (OPort, bitBucketOPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr, gsthunk, gsvCode, bcoCode)
import GSI.Eval (evalSync)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = bitBucketOPort >>= \ msg -> formatTestValue msg Nothing v (putStrLn . ($ ""))

formatTestValue :: OPort Message -> Maybe ProfCounter -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValue msg pc v@GSImplementationFailure{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc v@GSError{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc v@GSInvalidProgram{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc (GSThunk ts) k = do
    v <- evalSync msg pc [StackTrace $gshere []] ts
    formatTestValue msg pc v k
formatTestValue msg pc (GSClosure _ GSLambda{}) k = k $ ("<function>"++)
formatTestValue msg pc (GSClosure _ bco) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue (GSClosure _ "++) . (bcoCode bco++) . (") next>"++)
formatTestValue msg pc v@GSRecord{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc v@GSNatural{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc v@GSRune{} k = formatTestValueAtom msg pc v k
formatTestValue msg pc v@(GSConstr pos c [ GSThunk chts, s ]) k | c == gsvar ":" = do
    chv <- evalSync msg pc [StackTrace $gshere []] chts
    formatTestValue msg pc (GSConstr pos (gsvar ":") [ chv, s ]) k
formatTestValue msg pc v@(GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" = formatTestValueAtom msg pc v k
formatTestValue msg pc (GSConstr _ v as) k = formatArgs msg pc as $ \ ds -> k (fmtVarAtom v . ds)
formatTestValue msg pc v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue "++) . (gsvCode v++) . (" next>"++)

formatTestValueAtom :: OPort Message -> Maybe ProfCounter -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValueAtom msg pc (GSImplementationFailure pos msgs) k = k $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msgs++) . ('>':)
formatTestValueAtom msg pc (GSError err) k = k $ ('<':) . (fmtErrorShort err++) . ('>':)
formatTestValueAtom msg pc (GSInvalidProgram err) k = k $ ('<':) . (fmtInvalidProgram err++) . ('>':)
formatTestValueAtom msg pc (GSThunk ts) k = do
    v <- evalSync msg pc [StackTrace $gshere []] ts
    formatTestValueAtom msg pc v k
formatTestValueAtom msg pc (GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" =
    formatChar (GSRune ch) $ \ chds -> formatString msg pc s $ \ sds -> k (("qq{"++) . chds . sds . ('}':))
formatTestValueAtom msg pc (GSConstr pos c []) k = k $ fmtVarAtom c
formatTestValueAtom msg pc v@GSConstr{} k = formatTestValue msg pc v $ \ ds -> k $ ('(':) . ds . (')':)
formatTestValueAtom msg pc (GSRecord _ fs) k = case Map.null fs of
    True -> k $ ("〈〉"++)
    False -> formatFields msg pc (Map.assocs fs) $ \ ds -> k $ ('〈':) . (' ':) . ds . ('〉':)
formatTestValueAtom msg pc (GSNatural _ n) k = k $ shows n
formatTestValueAtom msg pc (GSRune r) k
    | r `elem` "/\\§()[]{}\n\t" = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom (GSRune "++) . shows r . (") next>"++)
    | otherwise = k $ ("r/"++) . (r:) . ('/':)
formatTestValueAtom msg pc v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom "++) . (gsvCode v++) . (" next>"++)

formatArgs :: OPort Message -> Maybe ProfCounter -> [GSValue] -> ((String -> String) -> IO a) -> IO a
formatArgs msg pc (x:xn) k = formatTestValueAtom msg pc x $ \ xds -> formatArgs msg pc xn $ \ xnds -> k $ (' ':) . xds . xnds
formatArgs msg pc [] k = k id

formatFields :: OPort Message -> Maybe ProfCounter -> [(GSVar, GSValue)] -> ((String -> String) -> IO a) -> IO a
formatFields msg pc ((v, x):fs) k = formatTestValue msg pc x $ \ xds -> formatFields msg pc fs $ \ fsds ->
    k $ fmtVarBindAtom v . (" = "++) . xds . ("; "++) . fsds
formatFields msg pc [] k = k id

formatChar :: GSValue -> ((String -> String) -> IO a) -> IO a
formatChar (GSRune '\n') k = k $ ("\\n"++)
formatChar (GSRune '\\') k = k $ ("\\\\"++)
formatChar (GSRune ch) k = k $ (ch:)
formatChar v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatChar "++) . (gsvCode v++) . (" next>"++)

formatString :: OPort Message -> Maybe ProfCounter -> GSValue -> ((String -> String) -> IO a) -> IO a
formatString msg pc (GSThunk ts) k = do
    v <- evalSync msg pc [StackTrace $gshere []] ts
    formatString msg pc v k
formatString msg pc (GSConstr _ c [ r0, s1 ]) k | c == gsvar ":" = formatChar r0 $ \ r0ds -> formatString msg pc s1 $ \ s1ds -> k $ r0ds . s1ds
formatString msg pc (GSConstr _ c []) k | c == gsvar "nil" = k $ id
formatString msg pc (GSConstr _ c as) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . fmtVarAtom c . (" next>"++)
formatString msg pc v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . (gsvCode v++) . (" next>"++)

printStackTrace :: GSValue -> IO ()
printStackTrace v = bitBucketOPort >>= \ msg -> w msg Nothing v
  where
    w msg pc (GSThunk ts) = do
        v <- evalSync msg pc [StackTrace $gshere []] ts
        w msg pc v
    w _ pc (GSImplementationFailure pos msg) = putStr $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msg++) . ('>':) $ "\n"
    w _ pc (GSClosure cs _) = putStr $ fmtCallers cs "\n"
    w _ pc gsv = error $ "printStackTrace " ++ gsvCode gsv ++ " next"
