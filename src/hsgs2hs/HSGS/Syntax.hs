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
        e <- expr env
        return $ SCArg ps e
    <|> do
        keyword "expr"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCExpr ps e
    <|> do
        pos <- getPos
        keyword "open-expr"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCOpenExpr pos ps e
    <|> do
        pos <- getPos
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

expr :: Env -> Parser Char Expr
expr env = empty
    <|> foldl EApp <$> exprAtom <*> many exprAtom
    <|> do
        pos <- getPos
        (v, parseHead, parseBody, parseElse) <- lambdalike env
        return (EVar pos v)
            <|> do
                h <- parseHead <* period
                b <- parseBody
                e <- parseElse
                return $ EVar pos v `EApp` h `EApp` b `EApp` e
  where
    exprAtom = empty
        <|> EVar <$> getPos <*> var env

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
  = EMissingCase Pos
  | EVar Pos String
  | EPat Pos Pattern
  | EOpen Pos Expr
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
whitespace = many (matching "whitespace" isSpace) *> return ()

globalEnv :: Env
globalEnv = Env{
    lambdas = Map.fromList [
        ("case", \ env -> (
            EPat <$> getPos <*> pattern,
            expr env,
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

eCode :: Expr -> String
eCode EMissingCase{} = "EMissingCase"
eCode EVar{} = "EVar"
eCode EPat{} = "EPat"
eCode EOpen{} = "EOpen"
eCode EApp{} = "EApp"

patCode :: Pattern -> String
patCode PView{} = "PView"
patCode PVar{} = "PVar"
patCode PApp{} = "PApp"
