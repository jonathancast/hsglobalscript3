{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), Param(..), interpolation, quote, scCode) where

import Control.Applicative (Alternative(..))

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)

import HSGS.Parser (Parser, notFollowedBy, matching, char, string, endBy)

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

param :: Parser Char Param
param = empty
    <|> do
        keyword "fvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ FVSParam vs

expr :: Parser Char Expr
expr = empty
    <|> exprAtom
  where
    exprAtom = empty
        <|> EVar <$> ident

data SourceComp
  = SCChar Char
  | SCImports
  | SCArg [Param] Expr

data Expr
  = EVar String

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