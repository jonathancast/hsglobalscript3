{-# LANGUAGE TemplateHaskell, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Control.Applicative (Alternative(..))

import Data.List (isSuffixOf)

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)

import GSI.Util (gsfatal)

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
   splitInput s >>=
   compileSource >>=
   formatOutput >>=
   return . concat . (("{-# LINE 1 " ++ show fn ++ " #-}\n"):)

mkHSFile :: FilePath -> FilePath
mkHSFile ".hsgs" = ".hs"
mkHSFile (c:s) = c:mkHSFile s
mkHSFile "" = ""

formatOutput :: [DestComp] -> Either String [String]
formatOutput (DCChar c:dcs) = ([c]:) <$> formatOutput dcs
formatOutput [] = return []
formatOutput dcs = $gsfatal $ "formatOutput " ++ show dcs ++ " next"

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource (SCChar c:scs) = (DCChar c:) <$> compileSource scs
compileSource [] = return []
compileSource scs = $gsfatal $ "compileSource " ++ show scs ++ " next"

splitInput :: String -> Either String [SourceComp]
splitInput ('$':s) = case parse interpolation s of
    Left err -> (SCChar '$':) <$> splitInput s
    Right (r, s') -> (r:) <$> splitInput s'
splitInput ('[':c:s) | c /= '|' = (SCChar '[':) <$> splitInput (c:s)
splitInput (c:s) | not (c `elem` "$[") = (SCChar c:) <$> splitInput s
splitInput "" = return []
splitInput s = $gsfatal $ "splitInput " ++ show s ++ " next"

interpolation :: Parser Char SourceComp
interpolation = empty

data SourceComp
  = SCChar Char
    deriving Show

data DestComp
  = DCChar Char
   deriving Show

parse :: Parser s a -> [s] -> Either String (a, [s])
parse p s = parse_w (runParser p $ $gsfatal "identity cont next") s where
    parse_w PPEmpty s = Left "parse error"
    parse_w p s = $gsfatal $ "parse " ++ pCode p ++ " next"

newtype Parser s a = Parser { runParser :: forall b. (a -> PrimParser s b) -> PrimParser s b }

data PrimParser s a
  = PPEmpty

instance Functor (Parser s) where
    fmap f px = $gsfatal $ "fmap f px next"

instance Applicative (Parser s) where
    pure x = $gsfatal "pure next"
    pf <*> px = $gsfatal $ "pf <*> px next"

instance Alternative (Parser s) where
    empty = Parser (\ k -> PPEmpty)
    p0 <|> p1 = $gsfatal $ "p0 <|> p1 next"

pCode :: PrimParser s a -> String
pCode PPEmpty = "PPEmpty"
