{-# LANGUAGE TemplateHaskell #-}
module TestHSGS (printTestExpr, printTestValue, printStackTrace) where

import Control.Monad (forever)

import qualified Data.Map as Map

import GSI.Util (StackTrace(..), gshere, fmtPos, fmtCallers)
import GSI.Syn (GSVar, gsvar, fmtVarAtom, fmtVarBindAtom)
import GSI.Error (fmtErrorShort, fmtInvalidProgram, fmtInvalidProgramShort)
import GSI.Message (Message)
import GSI.RTS (OPort, bitBucketOPort)
import GSI.Value (GSValue(..), GSBCO(..), GSExpr, gsthunk, gsvCode, bcoCode)
import GSI.Eval (evalSync)

printTestExpr :: GSExpr -> IO ()
printTestExpr e = $gsthunk e >>= printTestValue

printTestValue :: GSValue -> IO ()
printTestValue v = bitBucketOPort >>= \ msg -> formatTestValue msg v (putStrLn . ($ ""))

formatTestValue :: OPort Message -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValue msg v@GSImplementationFailure{} k = formatTestValueAtom msg v k
formatTestValue msg v@GSError{} k = formatTestValueAtom msg v k
formatTestValue msg v@GSInvalidProgram{} k = formatTestValueAtom msg v k
formatTestValue msg (GSThunk ts) k = do
    v <- evalSync msg [StackTrace $gshere []] ts
    formatTestValue msg v k
formatTestValue msg (GSClosure _ GSLambda{}) k = k $ ("<function>"++)
formatTestValue msg (GSClosure _ bco) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue (GSClosure _ "++) . (bcoCode bco++) . (") next>"++)
formatTestValue msg v@GSRecord{} k = formatTestValueAtom msg v k
formatTestValue msg v@GSNatural{} k = formatTestValueAtom msg v k
formatTestValue msg v@GSRune{} k = formatTestValueAtom msg v k
formatTestValue msg v@(GSConstr pos c [ GSThunk chts, s ]) k | c == gsvar ":" = do
    chv <- evalSync msg [StackTrace $gshere []] chts
    formatTestValue msg (GSConstr pos (gsvar ":") [ chv, s ]) k
formatTestValue msg v@(GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" = formatTestValueAtom msg v k
formatTestValue msg (GSConstr _ v as) k = formatArgs msg as $ \ ds -> k (fmtVarAtom v . ds)
formatTestValue msg v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValue "++) . (gsvCode v++) . (" next>"++)

formatTestValueAtom :: OPort Message -> GSValue -> ((String -> String) -> IO a) -> IO a
formatTestValueAtom msg (GSImplementationFailure pos msgs) k = k $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msgs++) . ('>':)
formatTestValueAtom msg (GSError err) k = k $ ('<':) . (fmtErrorShort err++) . ('>':)
formatTestValueAtom msg (GSInvalidProgram err) k = k $ ('<':) . (fmtInvalidProgram err++) . ('>':)
formatTestValueAtom msg (GSThunk ts) k = do
    v <- evalSync msg [StackTrace $gshere []] ts
    formatTestValueAtom msg v k
formatTestValueAtom msg (GSConstr _ c [ GSRune ch, s ]) k | c == gsvar ":" =
    formatChar (GSRune ch) $ \ chds -> formatString msg s $ \ sds -> k (("qq{"++) . chds . sds . ('}':))
formatTestValueAtom msg (GSConstr pos c []) k = k $ fmtVarAtom c
formatTestValueAtom msg v@GSConstr{} k = formatTestValue msg v $ \ ds -> k $ ('(':) . ds . (')':)
formatTestValueAtom msg (GSRecord _ fs) k = case Map.null fs of
    True -> k $ ("〈〉"++)
    False -> formatFields msg (Map.assocs fs) $ \ ds -> k $ ('〈':) . (' ':) . ds . ('〉':)
formatTestValueAtom msg (GSNatural n) k = k $ shows n
formatTestValueAtom msg (GSRune r) k
    | r `elem` "/\\§()[]{}\n\t" = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom (GSRune "++) . shows r . (") next>"++)
    | otherwise = k $ ("r/"++) . (r:) . ('/':)
formatTestValueAtom msg v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatTestValueAtom "++) . (gsvCode v++) . (" next>"++)

formatArgs :: OPort Message -> [GSValue] -> ((String -> String) -> IO a) -> IO a
formatArgs msg (x:xn) k = formatTestValueAtom msg x $ \ xds -> formatArgs msg xn $ \ xnds -> k $ (' ':) . xds . xnds
formatArgs msg [] k = k id

formatFields :: OPort Message -> [(GSVar, GSValue)] -> ((String -> String) -> IO a) -> IO a
formatFields msg ((v, x):fs) k = formatTestValue msg x $ \ xds -> formatFields msg fs $ \ fsds ->
    k $ fmtVarBindAtom v . (" = "++) . xds . ("; "++) . fsds
formatFields msg [] k = k id

formatChar :: GSValue -> ((String -> String) -> IO a) -> IO a
formatChar (GSRune '\n') k = k $ ("\\n"++)
formatChar (GSRune '\\') k = k $ ("\\\\"++)
formatChar (GSRune ch) k = k $ (ch:)
formatChar v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatChar "++) . (gsvCode v++) . (" next>"++)

formatString :: OPort Message -> GSValue -> ((String -> String) -> IO a) -> IO a
formatString msg (GSThunk ts) k = do
    v <- evalSync msg [StackTrace $gshere []] ts
    formatString msg v k
formatString msg (GSConstr _ c [ r0, s1 ]) k | c == gsvar ":" = formatChar r0 $ \ r0ds -> formatString msg s1 $ \ s1ds -> k $ r0ds . s1ds
formatString msg (GSConstr _ c []) k | c == gsvar "nil" = k $ id
formatString msg (GSConstr _ c as) k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . fmtVarAtom c . (" next>"++)
formatString msg v k = k $ ('<':) . fmtPos $gshere . ("unimpl: formatString "++) . (gsvCode v++) . (" next>"++)

printStackTrace :: GSValue -> IO ()
printStackTrace v = bitBucketOPort >>= \ msg -> w msg v
  where
    w msg (GSThunk ts) = do
        v <- evalSync msg [StackTrace $gshere []] ts
        w msg v
    w _ (GSImplementationFailure pos msg) = putStr $ ('<':) . fmtPos pos . ("Implementation Failure: "++) . (msg++) . ('>':) $ "\n"
    w _ (GSClosure cs _) = putStr $ fmtCallers cs "\n"
    w _ gsv = error $ "printStackTrace " ++ gsvCode gsv ++ " next"
