{-# LANGUAGE TemplateHaskell, Rank2Types, ExistentialQuantification #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Parser (Parser, parse, getPos, symbol, matching, char, string, notFollowedBy, (<?>), many1, endBy, pfail, Advanceable(..), advanceStr) where

import qualified Data.Set as Set

import Control.Applicative (Alternative(..))

import Data.Char (isPrint)
import Data.List (foldl')

import GSI.Util (Pos(..), gsfatal, fmtPos)

parse :: (Advanceable s, ParserDisplay s) => Parser s a -> Pos -> [s] -> Either String (a, Pos, [s])
parse p pos s = process $ parse_w (runParser p $ \ x -> PPReturnPlus x (PPFail [] [])) pos s where
    process :: ([(a, Pos, [s])], String) -> Either String (a, Pos, [s])
    process ([], err) = Left err
    process ([x], _) = Right x
    process (x:xn, err) = process (xn, err)

    parse_w :: (Advanceable s, ParserDisplay s) => PrimParser s a -> Pos -> [s] -> ([(a, Pos, [s])], String)
    parse_w (PPFail e0 e1) pos s = ([], fmtPos pos $ ("Unexpected "++) . displayString s $ fmtError e0 e1)
    parse_w (PPReturnPlus x p1) pos s = k $ parse_w p1 pos s where
        k ~(xn, err) = ((x, pos, s) : xn, err)
    parse_w (GetPos k) pos s = parse_w (k pos) pos s
    parse_w (Lookahead p) pos s = k (PPFail [] []) $ parse_w p pos s where
        k p0 ([], err) = ([], err)
        k p0 ([(p1, _, _)], _) = parse_w (p0 `or_w` p1) pos s
        k p0 ((p1, _, _):ps, err) = k (p0 `or_w` p1) (ps, err)
    parse_w (SymbolOrEof ek sk) pos [] = parse_w ek pos []
    parse_w (SymbolOrEof ek sk) pos (c:s') = case sk c of
        Left (e0, e1) -> ([], fmtPos pos $ ("Unexpected "++) . displayChar c $ fmtError e0 e1)
        Right p' -> parse_w p' (advance c pos) s'
    parse_w p pos s = $gsfatal $ "parse " ++ pCode p ++ " next"

    fmtError e0 e1 = concat (map ("; "++) e0) ++ fmt (Set.toList $ Set.fromList $ e1) where
        fmt [] = ""
        fmt [exp0] = "; expecting " ++ exp0
        fmt [exp0, exp1] = "; expecting " ++ exp0 ++ " or " ++ exp1
        fmt exp = "; expecting " ++ fmt' exp where
            fmt' [] = "<unknown>"
            fmt' [exp0] = " or " ++ exp0
            fmt' (exp0:exp) = exp0 ++ ", " ++ fmt' exp

pfail :: String -> Parser s a
pfail err = Parser (\ k -> PPFail [err] [])

getPos :: Parser s Pos
getPos = Parser (\ k -> GetPos k)

(<?>) :: Parser s a -> String -> Parser s a
p <?> s = Parser (\ k -> w (runParser p k)) where
    w (SymbolOrEof ek sk) = SymbolOrEof (w ek) (\ c -> case sk c of
        Left _ -> Left ([], [s])
        Right p1 -> Right p1
      )
    w p0 = $gsfatal $ pCode p0 ++ " <?> s next"

many1 :: Parser s a -> Parser s [a]
many1 p = (:) <$> p <*> many p

endBy :: Parser s a -> Parser s b -> Parser s [a]
p0 `endBy` p1 = many (p0 <* p1)

notFollowedBy :: Parser s a -> Parser s ()
notFollowedBy p = Parser (\ k -> k () `difference_w` runParser p (\ x -> PPReturnPlus x (PPFail [] []))) where
    difference_w :: PrimParser s a -> PrimParser s b -> PrimParser s a
    PPFail e0 e1 `difference_w` p1 = PPFail e0 e1
    PPReturnPlus x p0 `difference_w` p1 = Lookahead (negate_w p1 (PPReturnPlus x p0))
    Lookahead p0 `difference_w` SymbolOrEof ek1 sk1 = Lookahead (p0 `difference_w` SymbolOrEof ek1 sk1)
    GetPos k `difference_w` PPReturnPlus y p3 = PPFail [] []
    GetPos k `difference_w` Lookahead p1 = GetPos $ \ pos -> k pos `difference_w` Lookahead p1
    GetPos k `difference_w` SymbolOrEof ek1 sk1 = GetPos $ \ pos -> k pos `difference_w` SymbolOrEof ek1 sk1
    SymbolOrEof ek0 sk0 `difference_w` PPReturnPlus y p3 = PPFail [] []
    SymbolOrEof ek0 sk0 `difference_w` Lookahead p1 = Lookahead (SymbolOrEof ek0 sk0 `difference1_w` p1)
    SymbolOrEof ek0 sk0 `difference_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `difference_w` ek1) (\ ch ->
        case (sk0 ch, sk1 ch) of
            (Left exp0, Left exp1) -> Left exp0
            (Left exp0, Right p1') -> Left exp0
            (Right p0', Left exp1') -> Right p0'
            (Right p0', Right p1') -> Right $ p0' `difference_w` p1'
      )
    p0 `difference_w` p1 = $gsfatal $ pCode p0 ++ " `difference_w` " ++ pCode p1 ++ " next"

    difference1_w :: PrimParser s a -> PrimParser s (PrimParser s b) -> PrimParser s (PrimParser s a)
    p0 `difference1_w` PPFail e0 e1 = PPReturnPlus p0 $ PPFail [] []
    p0 `difference1_w` PPReturnPlus p1 pp1 = (p0 `difference_w` p1) `difference1_w` pp1
    p0 `difference1_w` SymbolOrEof ek1 sk1 = SymbolOrEof (p0 `difference1_w` ek1) (\ ch ->
        case sk1 ch of
            Left exp1 -> Right $ PPReturnPlus p0 $ PPFail [] []
            Right p1' -> Right $ p0 `difference1_w` p1'
      )
    p0 `difference1_w` p1 = $gsfatal $ "p0 `difference1_w` " ++ pCode p1 ++ " next"

    negate_w :: PrimParser s a -> b -> PrimParser s b
    negate_w (PPReturnPlus y p) x = PPFail [] []
    negate_w (SymbolOrEof ek sk) x = SymbolOrEof (negate_w ek x) (\c -> case sk c of
        Left _ -> Right $ PPReturnPlus x (PPFail [] [])
        Right p1 -> Right $ negate_w p1 x
      )
    negate_w p0 x = $gsfatal $ "negate_w " ++ pCode p0 ++ " next"

string :: String -> Parser Char ()
string s = mapM_ char s <?> displayString s ""

char :: Char -> Parser Char ()
char ch = matching (displayChar ch "") (==ch) *> return ()

symbol :: Parser s s
symbol = matching "symbol" (const True)

matching :: String -> (s -> Bool) -> Parser s s
matching cat p = Parser (\ k -> SymbolOrEof (PPFail [] [cat]) (\ c -> case p c of
    False -> Left ([], [cat])
    True -> Right (k c)
  ))

instance Functor (Parser s) where
    fmap f px = Parser (\ k -> runParser px (k . f))

instance Applicative (Parser s) where
    pure x = return x
    pf <*> px = pf >>= \ f -> px >>= \ x -> return (f x)

instance Alternative (Parser s) where
    empty = Parser (\ k -> PPFail [] [])
    p0 <|> p1 = Parser (\ k -> runParser p0 k `or_w` runParser p1 k) where

or_w :: PrimParser s a -> PrimParser s a -> PrimParser s a
PPFail e0 e1 `or_w` PPFail e2 e3 = PPFail (e0 ++ e2) (e1 ++ e3)
PPFail e0 e1 `or_w` PPReturnPlus x p1 = PPReturnPlus x (PPFail e0 e1 `or_w` p1)
PPFail e0 e1 `or_w` SymbolOrEof ek1 sk1 = SymbolOrEof (PPFail e0 e1 `or_w` ek1) (\ c -> case sk1 c of
    Left (e2, e3) -> Left (e0 ++ e2, e1 ++ e3)
    Right p1 -> Right p1
  )
PPFail e0 e1 `or_w` GetPos k1 = GetPos $ \ pos -> PPFail e0 e1 `or_w` k1 pos
PPReturnPlus x p0 `or_w` PPFail e2 e3 = PPReturnPlus x (p0 `or_w` PPFail e2 e3)
PPReturnPlus x p0 `or_w` PPReturnPlus y p1 = PPReturnPlus x (p0 `or_w` PPReturnPlus y p1)
PPReturnPlus x p0 `or_w` GetPos k1 = PPReturnPlus x (p0 `or_w` GetPos k1)
PPReturnPlus x p0 `or_w` SymbolOrEof ek1 sk1 = PPReturnPlus x (p0 `or_w` SymbolOrEof ek1 sk1)
GetPos k0 `or_w` PPFail e2 e3 = GetPos $ \ pos -> k0 pos `or_w` PPFail e2 e3
GetPos k0 `or_w` PPReturnPlus x p1 = PPReturnPlus x (GetPos k0 `or_w` p1)
GetPos k0 `or_w` GetPos k1 = GetPos $ \ pos -> k0 pos `or_w` k1 pos
GetPos k0 `or_w` Lookahead p1 = GetPos $ \ pos -> k0 pos `or_w` Lookahead p1
GetPos k0 `or_w` SymbolOrEof ek1 sk1 = GetPos $ \ pos -> k0 pos `or_w` SymbolOrEof ek1 sk1
Lookahead p0 `or_w` PPFail e2 e3 = Lookahead (mapPrim (`or_w` PPFail e2 e3) p0)
Lookahead p0 `or_w` PPReturnPlus x p1 = PPReturnPlus x (Lookahead p0 `or_w` p1)
Lookahead p0 `or_w` GetPos k1 = GetPos $ \ pos -> Lookahead p0 `or_w` k1 pos
Lookahead p0 `or_w` Lookahead p1 = Lookahead (p0 `or_w` p1)
Lookahead p0 `or_w` SymbolOrEof ek1 sk1 = Lookahead (p0 `or_w` PPReturnPlus (SymbolOrEof ek1 sk1) (PPFail [] []))
SymbolOrEof ek0 sk0 `or_w` PPFail e2 e3 = SymbolOrEof (ek0 `or_w` PPFail e2 e3) (\ c -> case sk0 c of
    Left (e0, e1) -> Left (e0 ++ e2, e1 ++ e3)
    Right p0 -> Right p0
  )
SymbolOrEof ek0 sk0 `or_w` PPReturnPlus x p1 = PPReturnPlus x (SymbolOrEof ek0 sk0 `or_w` p1)
SymbolOrEof ek0 sk0 `or_w` GetPos k = GetPos $ \ pos -> SymbolOrEof ek0 sk0 `or_w` k pos
SymbolOrEof ek0 sk0 `or_w` Lookahead p1 = Lookahead (PPReturnPlus (SymbolOrEof ek0 sk0) p1)
SymbolOrEof ek0 sk0 `or_w` SymbolOrEof ek1 sk1 = SymbolOrEof (ek0 `or_w` ek1) (\ c -> case (sk0 c, sk1 c) of
    (Left (e0, e1), Left (e2, e3)) -> Left (e0 ++ e2, e1 ++ e3)
    (Left err0, Right p1) -> Right p1
    (Right p0, Left err1) -> Right p0
    (Right p0, Right p1) -> Right $ p0 `or_w` p1
  )
p0 `or_w` p1 = $gsfatal $ pCode p0 ++ " <|> " ++ pCode p1 ++ " next"

mapPrim :: (a -> b) -> PrimParser s a -> PrimParser s b
mapPrim f (PPFail e0 e1) = PPFail e0 e1
mapPrim f (PPReturnPlus x p) = PPReturnPlus (f x) (mapPrim f p)
mapPrim f (SymbolOrEof ek sk) = SymbolOrEof (mapPrim f ek) (\ c -> case sk c of
    Left (e0, e1) -> Left (e0, e1)
    Right p1 -> Right (mapPrim f p1)
  )
mapPrim f p = $gsfatal $ "mapPrim f " ++ pCode p ++ " next"

instance Monad (Parser s) where
    return x = Parser (\ k -> k x)
    ax >>= f = Parser (\ k -> runParser ax $ \ x -> runParser (f x) k)

advanceStr :: String -> Pos -> Pos
advanceStr s pos = foldl' (flip advance) pos s

instance Advanceable Char where
    advance '\n' (Pos fn l c) = Pos fn (l+1) 1
    advance _ (Pos fn l c) = Pos fn l (c+1)

displayString :: ParserDisplay s => [s] -> String -> String
displayString s = ('"':) . w s where
    w [] = ('"':)
    w (c:s) = display '"' c . w s

displayChar :: ParserDisplay s => s -> String -> String
displayChar c = ('\'':) . display '\'' c . ('\'':)

instance ParserDisplay Char where
    display q ch | isPrint ch && ch /= q = (ch:)
    display q ch | q == ch = ('\\':) . (ch:)
    display q ch = $gsfatal $ "display " ++ show ch ++ " next"

newtype Parser s a = Parser { runParser :: forall b. (a -> PrimParser s b) -> PrimParser s b }

data PrimParser s a
  = PPFail [String] [String]
  | PPReturnPlus a (PrimParser s a)
  | GetPos (Pos -> PrimParser s a)
  | Lookahead (PrimParser s (PrimParser s a))
  | SymbolOrEof (PrimParser s a) (s -> Either ([String], [String]) (PrimParser s a))

class Advanceable r where
    advance :: r -> Pos -> Pos

class ParserDisplay r where
    display :: Char -> r -> String -> String

pCode :: PrimParser s a -> String
pCode PPFail{} = "PPFail"
pCode PPReturnPlus{} = "PPReturnPlus"
pCode GetPos{} = "GetPos"
pCode Lookahead{} = "Lookahead"
pCode SymbolOrEof{} = "SymbolOrEof"