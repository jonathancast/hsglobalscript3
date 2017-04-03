{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Pattern(..), Param(..), interpolation, quote, globalEnv, scCode, eCode, qloiCode, patCode) where

import Control.Applicative (Alternative(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)

import GSI.Util (Pos, gsfatal)

import HSGS.Parser (Parser, notFollowedBy, getPos, matching, symbol, char, string, endBy, pfail)

interpolation :: Parser Char SourceComp
interpolation = empty
    <|> do
        keyword "gsimports"
        return SCImports

quote :: Env -> Pos -> Parser Char SourceComp
quote env pos = empty
    <|> do
        keyword "arg"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCArg pos ps e
    <|> do
        keyword "expr"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCExpr ps e
    <|> do
        keyword "open-expr"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCOpenExpr pos ps e
    <|> do
        keyword "open-arg"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCOpenArg pos ps e
    <|> do
        keyword "pat"
        ps <- many param
        keywordOp "|"
        p <- pattern
        return $ SCPat ps p
    <|> do
        keyword "pat-arg"
        ps <- many param
        keywordOp "|"
        p <- pattern
        return $ SCPatArg pos ps p
    <|> do
        keyword "body"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCBody pos ps e

param :: Parser Char Param
param = empty
    <|> do
        keyword "hsvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ FVSParam vs

expr :: Env -> Parser Char Expr
expr env = empty
    <|> foldl (\ ef (pos, ex) -> EApp ef pos ex) <$> exprAtom <*> many ((,) <$> getPos <*> exprAtom)
    <|> do
        pos <- getPos
        (v, parseHead, parseBody, parseElse) <- lambdalike env
        return (EVar pos v)
            <|> do
                foldl (\ ef (pos1, ex) -> EApp ef pos1 ex) (EVar pos v) <$> sequence [
                    (,) <$> getPos <*> parseHead <* period,
                    (,) <$> getPos <*> parseBody,
                    (,) <$> getPos <*> parseElse
                  ]
  where
    exprAtom = empty
        <|> EVar <$> getPos <*> var env
        <|> do
            pos0 <- getPos
            (v, s) <- lexeme $ do
                v <- (:) <$> idStartChar <*> many idContChar
                s <- char '{' *> quoteItems env [] <* char '}'
                return (v, s)
            return $ EQLO pos0 v s

quoteItems :: Env -> [Char] -> Parser Char [QLOItem]
quoteItems env qs = empty
    <|> (if null qs then return [] else empty)
    <|> do
        pos <- getPos
        ch <- matching "open delimiter" (\ c -> c `Map.member` delimiters)
        (QChar pos ch :) <$> quoteItems env ((delimiters Map.! ch):qs)
    <|> case qs of
        [] -> empty
        ch:qs' -> do
            pos <- getPos
            char ch
            (QChar pos ch:) <$> quoteItems env qs'
    <|> (:) <$> (QChar <$> getPos <*> matching "ordinary character" (\ c -> not (c `elem` "()[]{}\\§"))) <*> quoteItems env qs
    <|> (:) <$> (QQChar <$> getPos <*> (char '\\' *> symbol)) <*> quoteItems env qs
    <|> (:) <$> (QInterpExpr <$> getPos <*> (char '§' *> char '(' *> expr env <* char ')')) <*> quoteItems env qs

delimiters = Map.fromList [ ('(', ')'), ('[', ']'), ('{', '}') ]

pattern :: Parser Char Pattern
pattern = empty
    <|> foldl PApp <$> patternAtom <*> many patternAtom
  where
    patternAtom = empty
        <|> PView <$> getPos <*> ident
        <|> PVar <$> getPos <*> (lexeme (char '\'') *> ident)

data SourceComp
  = SCChar Char
  | SCImports
  | SCArg Pos [Param] Expr
  | SCExpr [Param] Expr
  | SCOpenExpr Pos [Param] Expr
  | SCOpenArg Pos [Param] Expr
  | SCPat [Param] Pattern
  | SCPatArg Pos [Param] Pattern
  | SCBody Pos [Param] Expr

data Expr
  = EMissingCase Pos
  | EVar Pos String
  | EQLO Pos String [QLOItem]
  | EPat Pattern
  | EOpen Expr
  | EApp Expr Pos Expr

data QLOItem
  = QChar Pos Char
  | QQChar Pos Char
  | QInterpExpr Pos Expr

data Pattern
  = PVar Pos String
  | PView Pos String
  | PApp Pattern Pattern

data Param
  = FVSParam [String]

var :: Env -> Parser Char String
var env = lexeme $ do
    v <- (++) <$> alphaNumComp <*> (concat <$> many (char '-' *> (('-':) <$> alphaNumComp)))
    notFollowedBy (char '{')
    case Map.lookup v (lambdas env) of
        Nothing -> return v
        Just _ -> pfail $ v ++ " is not variable-like"

alphaNumComp :: Parser Char String
alphaNumComp = ((:) <$> idStartChar <*> many idContChar) <* notFollowedBy idContChar

lambdalike :: Env -> Parser Char (String, Parser Char Expr, Parser Char Expr, Parser Char Expr)
lambdalike env = lexeme $ do
    v <- (:) <$> idStartChar <*> many idContChar
    notFollowedBy idContChar
    case Map.lookup v (lambdas env) of
        Nothing -> pfail $ v ++ " is not lambda-like"
        Just f -> let (ph, pb, pe) = f env in return (v, ph, pb, pe)

ident :: Parser Char String
ident = lexeme $ ((:) <$> idStartChar <*> many idContChar) <* notFollowedBy idContChar

keyword :: String -> Parser Char ()
keyword s = lexeme $ string s <* notFollowedBy idContChar

idStartChar :: Parser Char Char
idStartChar = matching "identifier" isAlpha

idContChar :: Parser Char Char
idContChar = matching "identifier continuation character" isAlphaNum

keywordOp :: String -> Parser Char ()
keywordOp s = lexeme $ string s <* notFollowedBy opContChar

opContChar :: Parser Char Char
opContChar = matching "operator continuation character" isSymbol

comma :: Parser Char ()
comma = lexeme $ char ','

period :: Parser Char ()
period = lexeme $ char '.'

lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* whitespace

whitespace :: Parser Char ()
whitespace = empty
    <|> return ()
    <|> matching "whitespace" isSpace *> whitespace
    <|> string "--" *> many (char '-') *> notFollowedBy opContChar *> many (matching "character" (/='\n')) *> char '\n' *> whitespace

globalEnv :: Env
globalEnv = Env{
    lambdas = Map.fromList [
        ("case", \ env -> (
            EPat <$> pattern,
            EOpen <$> expr env,
            EMissingCase <$> getPos
        ))
    ]
  }

data Env = Env {
    lambdas :: Map String (Env -> (Parser Char Expr, Parser Char Expr, Parser Char Expr))
  }

scCode :: SourceComp -> String
scCode SCChar{} = "SCChar"
scCode SCImports{} = "SCImports"
scCode SCArg{} = "SCArg"
scCode SCExpr{} = "SCExpr"
scCode SCOpenExpr{} = "SCOpenExpr"
scCode SCOpenArg{} = "SCOpenArg"
scCode SCPat{} = "SCPat"
scCode SCPatArg{} = "SCPatArg"
scCode SCBody{} = "SCBody"

eCode :: Expr -> String
eCode EMissingCase{} = "EMissingCase"
eCode EVar{} = "EVar"
eCode EQLO{} = "EQLO"
eCode EPat{} = "EPat"
eCode EOpen{} = "EOpen"
eCode EApp{} = "EApp"

qloiCode :: QLOItem -> String
qloiCode QChar{} = "QChar"
qloiCode QQChar{} = "QQChar"
qloiCode QInterpExpr{} = "QInterpExpr"

patCode :: Pattern -> String
patCode PView{} = "PView"
patCode PVar{} = "PVar"
patCode PApp{} = "PApp"
