{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Control.Applicative (Alternative(..))

import Data.List (isSuffixOf)

import Data.Set (Set)
import qualified Data.Set as Set

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)

import GSI.Util (Pos(..), gsfatal, fmtPos)

import HSGS.Parser (Parser, parse, Advanceable(..), advanceStr)
import HSGS.Syntax (SourceComp(..), Expr(..), Param(..), interpolation, quote, scCode)
import HSGS.Output (DestComp(..), HSImport(..), HSExpr(..), dcCode, hsiCode, hsCode)

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
            Left err -> $gsfatal $ "main " ++ show err ++ " next"
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
formatExpr e = $gsfatal $ "formatExpr " ++ hsCode e ++ " next"

formatExprAtom :: HSExpr -> String -> String
formatExprAtom (HSConstr c) = (c++)
formatExprAtom (HSVar v) = (v++)
formatExprAtom (HSString s) = (show s++)
formatExprAtom (HSInteger n)
    | n >= 0 = (show n++)
    | otherwise = ('(':) . (show n++) . (')':)
formatExprAtom e@HSApp{} = ('(':) . formatExpr e . (')':)
formatExprAtom e = $gsfatal $ "formatExprAtom " ++ hsCode e ++ " next"

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource (SCChar c:scs) = (DCChar c:) <$> compileSource scs
compileSource (SCImports:scs) = do
    dcs <- compileSource scs
    return $ DCImports (gatherImports Set.empty dcs) : dcs
compileSource (SCArg ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileArg e <*> compileSource scs
compileSource (SCExpr ps e:scs) = (\ (is, e) dcs -> DCExpr is e : dcs) <$> compileExpr e <*> compileSource scs
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
compileExpr e = $gsfatal $ "compileExpr " ++ eCode e ++ " next"

hspos :: Pos -> HSExpr
hspos pos = HSConstr "Pos" `HSApp` HSString (filename pos) `HSApp` HSInteger (line pos) `HSApp` HSInteger (col pos)

splitInput :: Pos -> String -> Either String [SourceComp]
splitInput pos ('$':s) = case parse interpolation pos s of
    Left err -> (SCChar '$':) <$> splitInput (advance '$' pos) s
    Right (r, pos', s') -> (r:) <$> splitInput pos' s'
splitInput pos ('[':'g':'s':':':s) = case parse quote (advanceStr "[gs:" pos) s of
    Left err -> error err
    Right (r, pos', '|':']':s') -> (r:) <$> splitInput (advanceStr "|]" pos') s'
    Right (r, pos', s') -> error $ fmtPos pos' $ "Got " ++ show s' ++ "; expected \"|]\""
splitInput pos (c:s) = (SCChar c:) <$> splitInput (advance c pos) s
splitInput pos "" = return []

eCode :: Expr -> String
eCode EVar{} = "EVar"
