{-# LANGUAGE TemplateHaskell, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Control.Applicative (Alternative(..))

import Data.List (isSuffixOf)

import System.Directory (doesDirectoryExist, doesFileExist, getDirectoryContents)
import System.Environment (getArgs)

import GSI.Util (Pos(..), gsfatal, fmtPos)

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
formatOutput [] = return []
formatOutput dcs = $gsfatal $ "formatOutput " ++ show dcs ++ " next"

compileSource :: [SourceComp] -> Either String [DestComp]
compileSource (SCChar c:scs) = (DCChar c:) <$> compileSource scs
compileSource [] = return []
compileSource scs = $gsfatal $ "compileSource " ++ show scs ++ " next"

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

interpolation :: Parser Char SourceComp
interpolation = empty

quote :: Parser Char SourceComp
quote = empty
    <|> do
        keyword "arg"
        pfail "arg next"

data SourceComp
  = SCChar Char
    deriving Show

data DestComp
  = DCChar Char
   deriving Show

parse :: Parser s a -> Pos -> [s] -> Either String (a, Pos, [s])
parse p pos s = parse_w (runParser p $ $gsfatal "identity cont next") pos s where
    parse_w PPEmpty pos s = Left $ fmtPos pos $ "parse error"
    parse_w p pos s = $gsfatal $ fmtPos pos $ "parse " ++ pCode p ++ " next"

advanceStr :: String -> Pos -> Pos
advanceStr s pos = foldr advance pos s

advance :: Char -> Pos -> Pos
advance '\n' (Pos fn l c) = Pos fn (l+1) 1
advance _ (Pos fn l c) = Pos fn l (c+1)

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
    p0 <|> p1 = Parser (\ k -> runParser p0 k `or_w` runParser p1 k) where
        p0 `or_w` p1 = $gsfatal $ pCode p0 ++ " <|> " ++ pCode p1 ++ " next"

instance Monad (Parser s) where
    return x = $gsfatal "return next"
    ax >>= f = Parser (\ k -> runParser ax $ \ x -> runParser (f x) k)

pfail :: String -> Parser s a
pfail err = $gsfatal $ "pfail " ++ show err ++ " next"

keyword :: String -> Parser Char ()
keyword s = $gsfatal $ "keyword " ++ show s ++ " next"

lexeme :: Parser Char a -> Parser Char a
lexeme p = $gsfatal $ "lexeme p next"

pCode :: PrimParser s a -> String
pCode PPEmpty = "PPEmpty"
