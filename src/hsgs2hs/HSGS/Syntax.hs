{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), Param(..), interpolation, quote, scCode, eCode) where

import Control.Applicative (Alternative(..))

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)

import GSI.Util (Pos)

import HSGS.Parser (Parser, notFollowedBy, getPos, matching, char, string, endBy)

interpolation :: Parser Char SourceComp
interpolation = empty
    <|> do
        keyword "gsimports"
        return SCImports

quote :: Parser Char SourceComp
quote = empty
    <|> do
        keyword "arg"
        ps <- many param
        keywordOp "|"
        e <- expr
        return $ SCArg ps e
    <|> do
        keyword "expr"
        ps <- many param
        keywordOp "|"
        e <- expr
        return $ SCExpr ps e
    <|> do
        pos <- getPos
        keyword "open-expr"
        ps <- many param
        keywordOp "|"
        e <- expr
        return $ SCOpenExpr pos ps e
    <|> do
        pos <- getPos
        keyword "open-arg"
        ps <- many param
        keywordOp "|"
        e <- expr
        return $ SCOpenArg pos ps e
    <|> do
        keyword "pat"
        ps <- many param
        keywordOp "|"
        p <- pattern
        return $ SCPat ps p

param :: Parser Char Param
param = empty
    <|> do
        keyword "hsvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ FVSParam vs

expr :: Parser Char Expr
expr = empty
    <|> foldl EApp <$> exprAtom <*> many exprAtom
  where
    exprAtom = empty
        <|> EVar <$> getPos <*> ident

pattern :: Parser Char Pattern
pattern = empty

data SourceComp
  = SCChar Char
  | SCImports
  | SCArg [Param] Expr
  | SCExpr [Param] Expr
  | SCOpenExpr Pos [Param] Expr
  | SCOpenArg Pos [Param] Expr
  | SCPat [Param] Pattern

data Expr
  = EVar Pos String
  | EApp Expr Expr

data Pattern
  = PVar Pos String

data Param
  = FVSParam [String]

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

lexeme :: Parser Char a -> Parser Char a
lexeme p = p <* whitespace

whitespace :: Parser Char ()
whitespace = many (matching "whitespace" isSpace) *> return ()

scCode :: SourceComp -> String
scCode SCChar{} = "SCChar"
scCode SCImports{} = "SCImports"
scCode SCArg{} = "SCArg"
scCode SCExpr{} = "SCExpr"
scCode SCOpenExpr{} = "SCOpenExpr"
scCode SCOpenArg{} = "SCOpenArg"
scCode SCPat{} = "SCPat"

eCode :: Expr -> String
eCode EVar{} = "EVar"
eCode EApp{} = "EApp"
