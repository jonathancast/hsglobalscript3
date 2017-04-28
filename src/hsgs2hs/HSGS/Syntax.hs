{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Arg(..), Pattern(..), Generator(..), Param(..), interpolation, quote, globalEnv, scCode, eCode, qloiCode, argCode, patCode, genCode) where

import Control.Applicative (Alternative(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol, isPunctuation)

import GSI.Util (Pos, gsfatal)

import HSGS.Parser (Parser, notFollowedBy, getPos, matching, symbol, char, string, (<?>), endBy, pfail)

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
        p <- pattern env
        return $ SCPat ps p
    <|> do
        keyword "pat-arg"
        ps <- many param
        keywordOp "|"
        p <- pattern env
        return $ SCPatArg pos ps p
    <|> do
        keyword "body"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCBody pos ps e
    <|> do
        keyword "bind"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCBind pos ps e
    <|> do
        keyword "value"
        ps <- many param
        keywordOp "|"
        e <- expr env
        return $ SCValue pos ps e

param :: Parser Char Param
param = empty
    <|> do
        keyword "hsvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ HSVSParam vs
    <|> do
        keyword "fvs"
        keywordOp "="
        vs <- ident `endBy` comma
        return $ FVSParam vs

expr :: Env -> Parser Char Expr
expr env = empty
    <|> exprApp
    <|> lambdalike env Nothing
    <|> exprLeftList
  where
    exprLeftList = empty
        <|> do
            pos0 <- getPos
            op <- foldr (<|>) empty $ map (\ op -> keywordOp op *> return op) $ Map.keys $ leftops env
            pos1 <- getPos
            e <- exprApp <|> lambdalike env Nothing
            return $ EUnary pos0 op `EApp` ArgExpr pos1 e
    exprApp = foldl EApp <$> exprAtom <*> many exprArg
    exprAtom = empty
        <|> parens (expr env)
        <|> EVar <$> getPos <*> var env
        <|> ENumber <$> getPos <*> decimal
        <|> do
            pos0 <- getPos
            (v, s) <- lexeme $ do
                v <- (:) <$> idStartChar <*> many idContChar
                s <- char '{' *> quoteItems env [] <* char '}'
                return (v, s)
            return $ EQLO pos0 v s
    exprArg = empty
        <|> ArgExpr <$> getPos <*> exprAtom
        <|> ArgField <$> getPos <*> (keywordOp "#" *> ident)

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

pattern :: Env -> Parser Char Pattern
pattern env = empty
    <|> foldl PApp <$> patternAtom <*> many patternAtom
    <|> do
        px <- patternAtom
        posop <- getPos
        op <- operator env
        py <- patternAtom
        return $ PView posop op `PApp` px `PApp` py
  where
    patternAtom = empty
        <|> PView <$> getPos <*> ident
        <|> PVar <$> getPos <*> (lexeme (char '\'') *> ident)
        <|> PDiscard <$> (getPos <* underscoreTerm)

generator :: Env -> Parser Char Generator
generator env = empty
    <|> ExecGenerator <$> getPos <*> (underscoreTerm *> keywordOp "←" *> expr env)
    <|> BindGenerator <$> (lexeme (char '\'') *> ident) <*> getPos <*> (keywordOp "←" *> expr env)

data SourceComp
  = SCChar Char
  | SCPos Pos
  | SCImports
  | SCArg Pos [Param] Expr
  | SCExpr [Param] Expr
  | SCOpenExpr Pos [Param] Expr
  | SCOpenArg Pos [Param] Expr
  | SCPat [Param] Pattern
  | SCPatArg Pos [Param] Pattern
  | SCBody Pos [Param] Expr
  | SCBind Pos [Param] Expr
  | SCValue Pos [Param] Expr

data Expr
  = EMissingCase Pos
  | EVar Pos String
  | EUnary Pos String
  | ENumber Pos Integer
  | EQLO Pos String [QLOItem]
  | EPat Pattern
  | EImpGens [(Pos, Generator)] Pos
  | EMonadGens [(Pos, Generator)] Pos
  | EOpen Expr
  | EApp Expr Arg

data Arg
  = ArgExpr Pos Expr
  | ArgField Pos String

data QLOItem
  = QChar Pos Char
  | QQChar Pos Char
  | QInterpExpr Pos Expr

data Pattern
  = PVar Pos String
  | PView Pos String
  | PApp Pattern Pattern
  | PDiscard Pos

data Generator
  = ExecGenerator Pos Expr
  | BindGenerator String Pos Expr

data Param
  = HSVSParam [String]
  | FVSParam [String]

var :: Env -> Parser Char String
var env = lexeme $ do
    v <- identName
    notFollowedBy (char '{')
    case Map.lookup v (lambdas env) of
        Nothing -> return v
        Just _ -> pfail $ v ++ " is not variable-like"

operator :: Env -> Parser Char String
operator env = lexeme $ do
    v <- operatorComp
    return v

operatorComp :: Parser Char String
operatorComp = ((:) <$> opStartChar <*> many opContChar) <* notFollowedBy opContChar

lambdalike :: Env -> Maybe String -> Parser Char Expr
lambdalike env mbv = do
    pos <- getPos
    let vs = case mbv of
            Nothing -> Map.toList (lambdas env)
            Just v -> case Map.lookup v (lambdas env) of
                Nothing -> []
                Just f -> [(v, f)]
    (v, parseHead, parseBody, mbparseElse) <- foldr (<|>) empty $
        map (\ (v, f) -> keyword v *> let (ph, pb, pe) = f env in return (v, ph, pb, pe)) vs
    ef1 <- (EApp (EVar pos v) <$> (ArgExpr <$> getPos <*> parseHead)) <* period
    ef2 <- EApp ef1 <$> (ArgExpr <$> getPos <*> parseBody)
    ef3 <- case mbparseElse of
        Nothing -> return ef2
        Just pe -> EApp ef2 <$> (ArgExpr <$> getPos <*> pe)
    return ef3

ident :: Parser Char String
ident = lexeme identName

identName :: Parser Char String
identName = (++) <$> alphaNumComp <*> (concat <$> many ((:) <$> matching "separator" (`elem` "-.") <*> alphaNumComp))

alphaNumComp :: Parser Char String
alphaNumComp = ((:) <$> idStartChar <*> many idContChar) <* notFollowedBy idContChar

decimal :: Parser Char Integer
decimal = lexeme $ read <$> ((:) <$> digit <*> many digit) <* notFollowedBy digit

digit :: Parser Char Char
digit = matching "digit" (`elem` [ '0'..'9' ])

keyword :: String -> Parser Char ()
keyword s = lexeme $ string s <* notFollowedBy idContChar

idStartChar :: Parser Char Char
idStartChar = matching "identifier" isAlpha

idContChar :: Parser Char Char
idContChar = matching "identifier continuation character" isAlphaNum

keywordOp :: String -> Parser Char ()
keywordOp s = lexeme $ string s <* notFollowedBy opContChar

opStartChar :: Parser Char Char
opStartChar = matching "operator start character" (\ c -> isSymbol c || (isPunctuation c && not (c `elem` "()[]{}〈〉《》`_,;.'\"\\")))

opContChar :: Parser Char Char
opContChar = matching "operator continuation character" isSymbol

parens :: Parser Char a -> Parser Char a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

underscoreTerm = (lexeme $ char '_' *> notFollowedBy (idStartChar <|> matching "open delimiter" (`elem` "([{〈《")))
    <?> "variable-like underscore"

comma :: Parser Char ()
comma = lexeme $ char ','

semicolon :: Parser Char ()
semicolon = lexeme $ char ';'

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
        ("analyze", \ env -> (
            expr env,
            empty
                <|> EMissingCase <$> getPos
                <|> expr env
            ,
            Nothing
        )),
        ("analyze-impM", \ env -> (
            expr env,
            expr env,
            Nothing
        )),
        ("case", \ env -> (
            EPat <$> pattern env,
            EOpen <$> expr env,
            Just (empty
                <|> EMissingCase <$> getPos
                <|> lambdalike env (Just "case")
            )
        )),
        ("impfor", \ env -> (
            EImpGens <$> (((,) <$> getPos <*> generator env) `endBy` semicolon) <*> getPos,
            EOpen <$> expr env,
            Nothing
        )),
        ("either-for", \ env -> (
            EMonadGens <$> (((,) <$> getPos <*> generator env) `endBy` semicolon) <*> getPos,
            EOpen <$> expr env,
            Nothing
        )),
        ("parser-for", \ env -> (
            EMonadGens <$> (((,) <$> getPos <*> generator env) `endBy` semicolon) <*> getPos,
            EOpen <$> expr env,
            Nothing
        ))
    ],
    leftops = Map.fromList [
        ("<|>", ())
    ]
  }

data Env = Env {
    lambdas :: Map String (Env -> (Parser Char Expr, Parser Char Expr, Maybe (Parser Char Expr))),
    leftops :: Map String ()
  }

scCode :: SourceComp -> String
scCode SCChar{} = "SCChar"
scCode SCPos{} = "SCPos"
scCode SCImports{} = "SCImports"
scCode SCArg{} = "SCArg"
scCode SCExpr{} = "SCExpr"
scCode SCOpenExpr{} = "SCOpenExpr"
scCode SCOpenArg{} = "SCOpenArg"
scCode SCPat{} = "SCPat"
scCode SCPatArg{} = "SCPatArg"
scCode SCBody{} = "SCBody"
scCode SCBind{} = "SCBind"
scCode SCValue{} = "SCValue"

eCode :: Expr -> String
eCode EMissingCase{} = "EMissingCase"
eCode EVar{} = "EVar"
eCode EUnary{} = "EUnary"
eCode ENumber{} = "ENumber"
eCode EQLO{} = "EQLO"
eCode EPat{} = "EPat"
eCode EImpGens{} = "EImpGens"
eCode EMonadGens{} = "EMonadGens"
eCode EOpen{} = "EOpen"
eCode EApp{} = "EApp"

qloiCode :: QLOItem -> String
qloiCode QChar{} = "QChar"
qloiCode QQChar{} = "QQChar"
qloiCode QInterpExpr{} = "QInterpExpr"

argCode :: Arg -> String
argCode ArgExpr{} = "ArgExpr"
argCode ArgField{} = "ArgField"

patCode :: Pattern -> String
patCode PView{} = "PView"
patCode PVar{} = "PVar"
patCode PApp{} = "PApp"
patCode PDiscard{} = "PDiscard"

genCode :: Generator -> String
genCode ExecGenerator{} = "ExecGenerator"
genCode BindGenerator{} = "BindGenerator"
