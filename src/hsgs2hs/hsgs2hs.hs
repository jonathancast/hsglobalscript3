{-# LANGUAGE TemplateHaskell, Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}

import Control.Applicative (Alternative(..))

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)
import Data.List (foldl', isSuffixOf)

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
        ps <- many param
        pfail "arg next"

param :: Parser Char Param
param = empty
    <|> do
        keyword "fvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ FVSParam vs

data SourceComp
  = SCChar Char
    deriving Show

data Param
  = FVSParam [String]
     deriving Show

data DestComp
  = DCChar Char
   deriving Show

parse :: (Advanceable s, Show s) => Parser s a -> Pos -> [s] -> Either String (a, Pos, [s])
parse p pos s = parse_w (runParser p $ $gsfatal "identity cont next") pos s where
    parse_w PPEmpty pos s = Left $ fmtPos pos $ "parse error"
    parse_w (SymbolOrEof ek sk) pos [] = $gsfatal $ fmtPos pos $ "parse (SymbolOrEof ek sk) pos \"\" next"
    parse_w (SymbolOrEof ek sk) pos (c:s') = case sk c of
        Left exp -> Left $ fmtPos pos $ "Unexpected " ++ show c ++ "; expecting " ++ fmt exp where
            fmt [] = "<unknown>"
            fmt [exp0] = exp0
            fmt [exp0, exp1] = exp0 ++ " or " ++ exp1
            fmt exp = fmt' exp where
                fmt' [] = "<unknown>"
                fmt' [exp0] = " or " ++ exp0
                fmt' (exp0:exp) = exp0 ++ ", " ++ fmt' exp
        Right p' -> parse_w p' (advance c pos) s'
    parse_w p pos s = $gsfatal $ fmtPos pos $ "parse " ++ pCode p ++ " next"

advanceStr :: String -> Pos -> Pos
advanceStr s pos = foldl' (flip advance) pos s

class Advanceable r where
    advance :: r -> Pos -> Pos

instance Advanceable Char where
    advance '\n' (Pos fn l c) = Pos fn (l+1) 1
    advance _ (Pos fn l c) = Pos fn l (c+1)

newtype Parser s a = Parser { runParser :: forall b. (a -> PrimParser s b) -> PrimParser s b }

data PrimParser s a
  = PPEmpty
  | PPFail String
  | SymbolOrEof (PrimParser s a) (s -> Either [String] (PrimParser s a))

instance Functor (Parser s) where
    fmap f px = Parser (\ k -> runParser px (k . f))

instance Applicative (Parser s) where
    pure x = return x
    pf <*> px = pf >>= \ f -> px >>= \ x -> return (f x)

instance Alternative (Parser s) where
    empty = Parser (\ k -> PPEmpty)
    p0 <|> p1 = Parser (\ k -> runParser p0 k `or_w` runParser p1 k) where
        PPEmpty `or_w` p1 = p1
        p0 `or_w` PPFail err = p0
        SymbolOrEof ek0 sk0 `or_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `or_w` ek1) (\ c -> case (sk0 c, sk1 c) of
            (Left err0, Left err1) -> Left (err0 ++ err1)
            (Left err0, Right p1) -> Right p1
            (Right p0, Left err1) -> Right p0
            (Right p0, Right p1) -> $gsfatal "sk0 <|> sk1 next"
          )
        p0 `or_w` p1 = $gsfatal $ pCode p0 ++ " <|> " ++ pCode p1 ++ " next"

instance Monad (Parser s) where
    return x = Parser (\ k -> k x)
    ax >>= f = Parser (\ k -> runParser ax $ \ x -> runParser (f x) k)

pfail :: String -> Parser s a
pfail err = Parser (\ k -> PPFail err)

(<?>) :: Parser s a -> String -> Parser s a
p <?> s = Parser (\ k -> w (runParser p k)) where
    w (SymbolOrEof ek sk) = SymbolOrEof (w ek) (\ c -> case sk c of
        Left _ -> Left [s]
        Right p1 -> Right p1
      )
    w p0 = $gsfatal $ pCode p0 ++ " <?> s next"

endBy :: Parser s a -> Parser s b -> Parser s [a]
p0 `endBy` p1 = many (p0 <* p1)

keyword :: String -> Parser Char ()
keyword s = lexeme $ string s <* notFollowedBy idContChar

ident :: Parser Char String
ident = ((:) <$> idStartChar <*> many idContChar) <* notFollowedBy idContChar

keywordOp :: String -> Parser Char ()
keywordOp s = lexeme $ string s <* notFollowedBy opContChar

idStartChar :: Parser Char Char
idStartChar = matching "identifier" isAlpha

idContChar :: Parser Char Char
idContChar = matching "identifier continuation character" isAlphaNum 

opContChar :: Parser Char Char
opContChar = matching "operator continuation character" isSymbol

string :: String -> Parser Char ()
string s = mapM_ char s <?> show s

comma :: Parser Char ()
comma = lexeme $ char ','

char :: Char -> Parser Char ()
char ch = matching (show ch) (==ch) *> return ()

lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* whitespace

whitespace :: Parser Char ()
whitespace = many (matching "whitespace" isSpace) *> return ()

notFollowedBy :: Parser s a -> Parser s ()
notFollowedBy p = Parser (\ k -> k () `difference_w` runParser p ($gsfatal "return next")) where
    difference_w :: PrimParser s a -> PrimParser s b -> PrimParser s a
    SymbolOrEof ek0 sk0 `difference_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `difference_w` ek1) (\ ch ->
        case (sk0 ch, sk1 ch) of
            (Left exp0, Left exp1) -> Left exp0
            (Left exp0, Right p1') -> Left exp0
            (Right p0', Left exp1') -> Right p0'
            (Right p0', Right p1') -> $gsfatal "sk0 `difference_w` sk1 next"
      )
    p0 `difference_w` p1 = $gsfatal $ pCode p0 ++ " `difference_w` " ++ pCode p1 ++ " next"

matching :: String -> (s -> Bool) -> Parser s s
matching cat p = Parser (\ k -> SymbolOrEof ($gsfatal "matching next") (\ c -> case p c of
    False -> Left [cat]
    True -> Right (k c)
  ))

pCode :: PrimParser s a -> String
pCode PPEmpty{} = "PPEmpty"
pCode PPFail{} = "PPFail"
pCode SymbolOrEof{} = "SymbolOrEof"
