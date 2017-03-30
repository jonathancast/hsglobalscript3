{-# LANGUAGE TemplateHaskell, Rank2Types, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Parser (Parser, parse, getPos, matching, char, string, notFollowedBy, endBy, Advanceable(..), advanceStr) where

import Control.Applicative (Alternative(..))

import Data.List (foldl')

import GSI.Util (Pos(..), gsfatal, fmtPos)

parse :: (Advanceable s, Show s) => Parser s a -> Pos -> [s] -> Either String (a, Pos, [s])
parse p pos s = parse_w id (runParser p $ \ x -> PPReturnPlus x PPEmpty) pos s where
    parse_w :: (Advanceable s, Show s) => (Either String (a, Pos, [s]) -> Either String (a, Pos, [s])) -> PrimParser s a -> Pos -> [s] -> Either String (a, Pos, [s])
    parse_w k PPEmpty pos s = k $ Left $ fmtPos pos $ "Unexpected " ++ show s
    parse_w k (NotFollowedByOr x p0 p1) pos s = parse_w k' p1 pos s where
        k' (Right x) = Right x -- Longer match, so go with that
        k' (Left _) = case parse_w id p0 pos s of -- Check that there is no match of p0
            Left _ -> Right (x, pos, s)
            Right _ -> k $ Left $ fmtPos pos $ "parse error"
    parse_w k (PPReturnPlus x p1) pos s = parse_w k' p1 pos s where
        k' (Right y) = Right y -- Longer match, so go with that
        k' (Left _) = Right (x, pos, s) -- Fall back to this match
    parse_w k (GetPos k') pos s = parse_w k (k' pos) pos s
    parse_w k (SymbolOrEof ek sk) pos [] = $gsfatal $ fmtPos pos $ "parse (SymbolOrEof ek sk) pos \"\" next"
    parse_w k (SymbolOrEof ek sk) pos (c:s') = case sk c of
        Left exp -> k $ Left $ fmtPos pos $ "Unexpected " ++ show c ++ "; expecting " ++ fmt exp where
            fmt [] = "<unknown>"
            fmt [exp0] = exp0
            fmt [exp0, exp1] = exp0 ++ " or " ++ exp1
            fmt exp = fmt' exp where
                fmt' [] = "<unknown>"
                fmt' [exp0] = " or " ++ exp0
                fmt' (exp0:exp) = exp0 ++ ", " ++ fmt' exp
        Right p' -> parse_w k p' (advance c pos) s'
    parse_w k p pos s = $gsfatal $ fmtPos pos $ "parse " ++ pCode p ++ " next"

pfail :: String -> Parser s a
pfail err = Parser (\ k -> PPFail err)

getPos :: Parser s Pos
getPos = Parser (\ k -> GetPos k)

(<?>) :: Parser s a -> String -> Parser s a
p <?> s = Parser (\ k -> w (runParser p k)) where
    w (SymbolOrEof ek sk) = SymbolOrEof (w ek) (\ c -> case sk c of
        Left _ -> Left [s]
        Right p1 -> Right p1
      )
    w p0 = $gsfatal $ pCode p0 ++ " <?> s next"

endBy :: Parser s a -> Parser s b -> Parser s [a]
p0 `endBy` p1 = many (p0 <* p1)

notFollowedBy :: Parser s a -> Parser s ()
notFollowedBy p = Parser (\ k -> k () `difference_w` runParser p (\ x -> PPReturnPlus x PPEmpty)) where
    difference_w :: PrimParser s a -> PrimParser s b -> PrimParser s a
    PPFail err `difference_w` p1 = PPFail err
    PPReturnPlus x p0 `difference_w` p1 = NotFollowedByOr x p1 (p0 `difference_w` p1)
    GetPos k `difference_w` SymbolOrEof ek1 sk1 = GetPos $ \ pos -> k pos `difference_w` SymbolOrEof ek1 sk1
    SymbolOrEof ek0 sk0 `difference_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `difference_w` ek1) (\ ch ->
        case (sk0 ch, sk1 ch) of
            (Left exp0, Left exp1) -> Left exp0
            (Left exp0, Right p1') -> Left exp0
            (Right p0', Left exp1') -> Right p0'
            (Right p0', Right p1') -> Right $ p0' `difference_w` p1'
      )
    NotFollowedByOr x p1 p2 `difference_w` PPReturnPlus y p3 = PPEmpty
    p0 `difference_w` p1 = $gsfatal $ pCode p0 ++ " `difference_w` " ++ pCode p1 ++ " next"

string :: String -> Parser Char ()
string s = mapM_ char s <?> show s

char :: Char -> Parser Char ()
char ch = matching (show ch) (==ch) *> return ()

matching :: String -> (s -> Bool) -> Parser s s
matching cat p = Parser (\ k -> SymbolOrEof ($gsfatal "matching next") (\ c -> case p c of
    False -> Left [cat]
    True -> Right (k c)
  ))

instance Functor (Parser s) where
    fmap f px = Parser (\ k -> runParser px (k . f))

instance Applicative (Parser s) where
    pure x = return x
    pf <*> px = pf >>= \ f -> px >>= \ x -> return (f x)

instance Alternative (Parser s) where
    empty = Parser (\ k -> PPEmpty)
    p0 <|> p1 = Parser (\ k -> runParser p0 k `or_w` runParser p1 k) where
        PPEmpty `or_w` p1 = p1
        p0 `or_w` PPEmpty = p0
        p0 `or_w` PPFail err = p0
        GetPos k0 `or_w` PPReturnPlus x p1 = PPReturnPlus x (GetPos k0 `or_w` p1)
        GetPos k0 `or_w` GetPos k1 = GetPos $ \ pos -> k0 pos `or_w` k1 pos
        GetPos k0 `or_w` SymbolOrEof ek1 sk1 = GetPos $ \ pos -> k0 pos `or_w` SymbolOrEof ek1 sk1
        SymbolOrEof ek0 sk0 `or_w` PPReturnPlus x p1 = PPReturnPlus x (SymbolOrEof ek0 sk0 `or_w` p1)
        SymbolOrEof ek0 sk0 `or_w` GetPos k = GetPos $ \ pos -> SymbolOrEof ek0 sk0 `or_w` k pos
        SymbolOrEof ek0 sk0 `or_w` NotFollowedByOr x p1 p2 = NotFollowedByOr x p1 (SymbolOrEof ek0 sk0 `or_w` p2)
        SymbolOrEof ek0 sk0 `or_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `or_w` ek1) (\ c -> case (sk0 c, sk1 c) of
            (Left err0, Left err1) -> Left (err0 ++ err1)
            (Left err0, Right p1) -> Right p1
            (Right p0, Left err1) -> Right p0
            (Right p0, Right p1) -> Right $ p0 `or_w` p1
          )
        p0 `or_w` p1 = $gsfatal $ pCode p0 ++ " <|> " ++ pCode p1 ++ " next"

instance Monad (Parser s) where
    return x = Parser (\ k -> k x)
    ax >>= f = Parser (\ k -> runParser ax $ \ x -> runParser (f x) k)

advanceStr :: String -> Pos -> Pos
advanceStr s pos = foldl' (flip advance) pos s

instance Advanceable Char where
    advance '\n' (Pos fn l c) = Pos fn (l+1) 1
    advance _ (Pos fn l c) = Pos fn l (c+1)

newtype Parser s a = Parser { runParser :: forall b. (a -> PrimParser s b) -> PrimParser s b }

data PrimParser s a
  = PPEmpty
  | PPFail String
  | PPReturnPlus a (PrimParser s a)
  | GetPos (Pos -> PrimParser s a)
  | forall b. NotFollowedByOr a (PrimParser s b) (PrimParser s a)
  | SymbolOrEof (PrimParser s a) (s -> Either [String] (PrimParser s a))

class Advanceable r where
    advance :: r -> Pos -> Pos

pCode :: PrimParser s a -> String
pCode PPEmpty{} = "PPEmpty"
pCode PPFail{} = "PPFail"
pCode PPReturnPlus{} = "PPReturnPlus"
pCode GetPos{} = "GetPos"
pCode NotFollowedByOr{} = "NotFollowedByOr"
pCode SymbolOrEof{} = "SymbolOrEof"