{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Control.Applicative (Alternative(..))

import Data.List (isSuffixOf)

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import System.IO (hPutStrLn, stderr)

import GSI.Util (Pos(..), gsfatal, fmtPos)

import HSGS.Parser (Parser, parse, Advanceable(..), advanceStr)
import HSGS.Syntax (SourceComp(..), Expr(..), Pattern(..), Param(..), interpolation, quote, scCode, eCode, patCode)
import HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode)

import qualified HSGS.Syntax as Syntax

main = do
    as <- getArgs
    mapM processArg as

processArg a = do
    id <- doesDirectoryExist a
    irf <- doesFileExist a
    if id then do
        as <- getDirectoryContents a
        mapM_ processArg $ map (\ a' -> a ++ '/' : a') $ filter (\ a' -> a' /= "." && a' /= "..") as
      else if irf && ".hsgs" `isSuffixOf` a then do
        s <- readFile a
        case compileHSGSSource a s of
            Left err -> do
                hPutStrLn stderr err
                exitWith $ ExitFailure 1
            Right s' -> do
                writeFile (mkHSFile a) s'
      else do
        return ()

compileHSGSSource :: FilePath -> String -> Either String String
compileHSGSSource fn s =
   splitInput (Pos fn 1 1) s >>=
   compileSource >>=
   formatOutput >>=
   return . concat . (("{-# LINE 1 " ++ show fn ++ " #-}\n"):)

mkHSFile :: FilePath -> FilePath
mkHSFile ".hsgs" = ".hs"
mkHSFile (c:s) = c:mkHSFile s
mkHSFile "" = ""

formatOutput :: [DestComp] -> Either String [String]
formatOutput (DCChar c:dcs) = ([c]:) <$> formatOutput dcs
formatOutput (DCImports is:dcs) = (map formatImport (Set.toList is)++) <$> formatOutput dcs
formatOutput (DCExpr _ e:dcs) = ((formatExprAtom e ""):) <$> formatOutput dcs
formatOutput (dc:dcs) = $gsfatal $ "formatOutput " ++ dcCode dc ++ " next"
formatOutput [] = return []

formatImport :: HSImport -> String
formatImport (HSIType m t) = "import " ++ m ++ " (" ++ t ++ "(..))\n"
formatImport (HSIVar m v) = "import " ++ m ++ " (" ++ v ++ ")\n"
formatImport hs = $gsfatal $ "formatImport " ++ hsiCode hs ++ " next"

formatExpr :: HSExpr -> String -> String
formatExpr e@HSConstr{} = formatExprAtom e
formatExpr e@HSVar{} = formatExprAtom e
formatExpr (HSApp ef ex) = formatExpr ef . (' ':) . formatExprAtom ex
formatExpr (HSLambda vs e) = ('\\':) . foldr (.) id (map (\ v -> (' ':) . (v++)) vs) . (" -> "++) . formatExpr e
formatExpr e = $gsfatal $ "formatExpr " ++ hsCode e ++ " next"

formatExprAtom :: HSExpr -> String -> String
formatExprAtom (HSConstr c) = (c++)
formatExprAtom (HSVar v) = (v++)
formatExprAtom (HSString s) = (show s++)
formatExprAtom (HSInteger n)
    | n >= 0 = (show n++)
    | otherwise = ('(':) . (show n++) . (')':)
formatExprAtom e@HSApp{} = ('(':) . formatExpr e . (')':)
formatExprAtom e@HSLambda{} = ('(':) . formatExpr e . (')':)
formatExprAtom (HSList es) = fmt es where
    fmt [] = ("[]"++)
    fmt es = ("[ "++) . fmt' es
    fmt' [] = (" ]"++) -- Actually dead code, but keep GHC quiet
    fmt' [ e ] = formatExpr e . (" ]"++)
    fmt' (e:es) = formatExpr e . (", "++) . fmt' es
formatExprAtom e = $gsfatal $ "formatExprAtom " ++ hsCode e ++ " next"

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource (SCChar c:scs) = (DCChar c:) <$> compileSource scs
compileSource (SCImports:scs) = do
    dcs <- compileSource scs
    return $ DCImports (gatherImports Set.empty dcs) : dcs
compileSource (SCArg ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileArg e <*> compileSource scs
compileSource (SCExpr ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileExpr e <*> compileSource scs
compileSource (SCOpenExpr pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileOpenExpr pos e <*> compileSource scs
compileSource (SCOpenArg pos ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileOpenArg pos e <*> compileSource scs
compileSource (SCPat ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePat globalEnv p <*> compileSource scs
compileSource (SCPatArg pos ps p:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compilePatArg globalEnv pos p <*> compileSource scs
compileSource (sc:scs) = $gsfatal $ "compileSource " ++ scCode sc ++ " next"
compileSource [] = return []

gatherImports :: Set HSImport -> [DestComp] -> Set HSImport
gatherImports is (DCChar _:dcs) = gatherImports is dcs
gatherImports is (DCExpr is' _:dcs) = gatherImports (is `Set.union` is') dcs
gatherImports is (dc:dcs) = $gsfatal $ "gatherImports " ++ dcCode dc ++ " next"
gatherImports is [] = is

compileArg :: Expr -> Either String (Set HSImport, HSExpr)
compileArg (EVar _ v) = return (
    Set.singleton (HSIType "GSI.Value" "GSArg"),
    HSConstr "GSArgVar" `HSApp` HSVar v
  )
compileArg e = $gsfatal $ "compileArg " ++ eCode e ++ " next"

compileExpr :: Expr -> Either String (Set HSImport, HSExpr)
compileExpr (EVar pos v) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcenter_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcenter_w" `HSApp` (hspos pos) `HSApp` HSVar v
  )
compileExpr (EApp f e) = compileApp f [e]
compileExpr e = $gsfatal $ "compileExpr " ++ eCode e ++ " next"

compileOpenExpr :: Pos -> Expr -> Either String (Set HSImport, HSExpr)
compileOpenExpr pos e = do
    (is, hse) <- compileExpr e
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcarg_w", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSVar "gsbcarg_w" `HSApp` hspos pos `HSApp` HSLambda ["env"] hse
      )

compileOpenArg :: Pos -> Expr -> Either String (Set HSImport, HSExpr)
compileOpenArg pos e = do
    (is, hse) <- compileOpenExpr pos e
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` hse
      )

compileApp :: Expr -> [Expr] -> Either String (Set HSImport, HSExpr)
compileApp (EVar pos f) as = do
    as' <- mapM compileArg as
    return (
        Set.unions $
            Set.fromList [ HSIVar "GSI.ByteCode" "gsbcapply_w", HSIType "GSI.Util" "Pos" ] :
            map (\ (is, _) -> is) as'
        ,
        HSVar "gsbcapply_w" `HSApp` hspos pos `HSApp` HSVar f `HSApp` HSList (map (\ (_, a) -> a) as')
      )
compileApp (EApp f a) as = compileApp f (a:as)
compileApp f as = $gsfatal $ "compileApp " ++ eCode f ++ " next"

compilePat :: Env -> Pattern -> Either String (Set HSImport, HSExpr)
compilePat env (PVar pos v) = return (
    Set.fromList [ HSIVar "GSI.ByteCode" "gsbcvarpattern_w", HSIType "GSI.Util" "Pos" ],
    HSVar "gsbcvarpattern_w" `HSApp` hspos pos `HSApp` HSString v
  )
compilePat env (PApp p0 p1) = compilePatApp env p0 [p1]
compilePat env p = $gsfatal $ "compilePat " ++ patCode p ++ " next"

compilePatArg :: Env -> Pos -> Pattern -> Either String (Set HSImport, HSExpr)
compilePatArg env pos p = do
    (is, e) <- compilePat env p
    return (
        Set.fromList [ HSIType "GSI.Value" "GSArg", HSIType "GSI.Util" "Pos" ] `Set.union` is,
        HSConstr "GSArgExpr" `HSApp` hspos pos `HSApp` e
      )

compilePatApp :: Env -> Pattern -> [Pattern] -> Either String (Set HSImport, HSExpr)
compilePatApp env (PView pos v) as = do
    as' <- mapM (compilePat env) as
    (isv, ev) <- case Map.lookup v (gsviews env) of
        Nothing -> Left $ fmtPos pos $ "view " ++ v ++ " not in scope"
        Just (isv, ev) -> return (isv, ev)
    return (
        Set.fromList [ HSIVar "GSI.ByteCode" "gsbcviewpattern_w", HSIType "GSI.Util" "Pos" ] `Set.union` isv `Set.union` Set.unions (map (\ (is, _) -> is) as'),
        foldl HSApp (HSVar "gsbcviewpattern_w" `HSApp` hspos pos `HSApp` ev) (map (\ (_, e) -> e) as')
      )
compilePatApp env p as = $gsfatal $ "compilePatApp " ++ patCode p ++ " next"

hspos :: Pos -> HSExpr
hspos pos = HSConstr "Pos" `HSApp` HSString (filename pos) `HSApp` HSInteger (line pos) `HSApp` HSInteger (col pos)

splitInput :: Pos -> String -> Either String [SourceComp]
splitInput pos ('$':s) = case parse interpolation pos s of
    Left err -> (SCChar '$':) <$> splitInput (advance '$' pos) s
    Right (r, pos', s') -> (r:) <$> splitInput pos' s'
splitInput pos ('[':'g':'s':':':s) = case parse (quote Syntax.globalEnv) (advanceStr "[gs:" pos) s of
    Left err -> error err
    Right (r, pos', '|':']':s') -> (r:) <$> splitInput (advanceStr "|]" pos') s'
    Right (r, pos', s') -> error $ fmtPos pos' $ "Got " ++ show s' ++ "; expected \"|]\""
splitInput pos (c:s) = (SCChar c:) <$> splitInput (advance c pos) s
splitInput pos "" = return []

globalEnv :: Env
globalEnv = Env{
    gsviews = Map.fromList [
        ("right", (Set.singleton $ HSIVar "GSI.Either" "gsright_view", HSVar "gsright_view"))
      ]
  }

data Env = Env {
    gsviews :: Map String (Set HSImport, HSExpr)
  }
