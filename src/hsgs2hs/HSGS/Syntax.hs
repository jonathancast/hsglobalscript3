{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), Pattern(..), Param(..), interpolation, quote, globalEnv, scCode, eCode, patCode) where

import Control.Applicative (Alternative(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol)

import GSI.Util (Pos, gsfatal)

import HSGS.Parser (Parser, notFollowedBy, getPos, matching, char, string, endBy, pfail)

interpolation :: Parser Char SourceComp
interpolation = empty
    <|> do
        keyword "gsimports"
        return SCImports

quote :: Env -> Parser Char SourceComp
quote env = empty
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
    <|> do
        pos <- getPos
        keyword "pat-arg"
        ps <- many param
        keywordOp "|"
        p <- pattern
        return $ SCPatArg pos ps p

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
    <|> foldl PApp <$> patternAtom <*> many patternAtom
  where
    patternAtom = empty
        <|> PView <$> getPos <*> ident
        <|> PVar <$> getPos <*> (lexeme (char '\'') *> ident)

data SourceComp
  = SCChar Char
  | SCImports
  | SCArg [Param] Expr
  | SCExpr [Param] Expr
  | SCOpenExpr Pos [Param] Expr
  | SCOpenArg Pos [Param] Expr
  | SCPat [Param] Pattern
  | SCPatArg Pos [Param] Pattern

data Expr
  = EVar Pos String
  | EApp Expr Expr

data Pattern
  = PVar Pos String
  | PView Pos String
  | PApp Pattern Pattern

data Param
  = FVSParam [String]

var :: Env -> Parser Char String
var env = lexeme $ do
    v <- (:) <$> idStartChar <*> many idContChar
    notFollowedBy idContChar
    case Map.lookup v (lambdas env) of
        Nothing -> return v
        Just _ -> pfail $ v ++ " is not variable-like"

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

globalEnv :: Env
globalEnv = Env{
    lambdas = Map.empty
  }

data Env = Env {
    lambdas :: Map String ()
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

eCode :: Expr -> String
eCode EVar{} = "EVar"
eCode EApp{} = "EApp"

patCode :: Pattern -> String
patCode PView{} = "PView"
patCode PVar{} = "PVar"
patCode PApp{} = "PApp"