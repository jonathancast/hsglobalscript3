{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-overlapping-patterns -fwarn-incomplete-patterns #-}
module HSGS.Syntax (SourceComp(..), Expr(..), QLOItem(..), Arg(..), Pattern(..), Generator(..), Param(..), interpolation, quote, globalEnv, scCode, eCode, qloiCode, argCode, patCode, genCode) where

import Control.Applicative (Alternative(..))

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (isAlpha, isAlphaNum, isSpace, isSymbol, isPunctuation)

import GSI.Util (Pos, gsfatal)

import HSGS.Parser (Parser, notFollowedBy, getPos, matching, symbol, char, string, (<?>), many1, endBy, pfail)

interpolation :: Parser Char SourceComp
interpolation = empty
    <|> do
        keyword "gsimports"
        return SCImports
    <|> do
        keyword "gsdeclare"
        gsv <- ident
        hsv <- hsident
        return $ SCDeclareVar gsv hsv
    <|> do
        keyword "gsdeclare"
        keyword "view"
        gsv <- ident
        hsv <- hsident
        return $ SCDeclareView gsv hsv

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
    <|> exprOp
    <|> exprLeftList
  where
    exprOp = empty
        <|> do
            pos0 <- getPos
            e0 <- exprApp
            empty
                <|> return e0
                <|> do
                    oes <- parseOps $ leftops env
                    return $ foldl (eleftop pos0) e0 oes
                <|> do
                    oes <- parseOps $ rightops env
                    let (posop, op, pos1, e1) = foldr1 erightop oes
                    return $ EVar posop op `EApp` ArgExpr pos0 e0 `EApp` ArgExpr pos1 e1
                <|> do
                    pos1 <- getPos
                    op <- foldr (<|>) empty $ map (\ op -> keywordOp op *> return op) $ Map.keys $ nonops env
                    pos2 <- getPos
                    e1 <- exprApp <|> lambdalike env Nothing
                    return $ EVar pos1 op `EApp` ArgExpr pos0 e0 `EApp` ArgExpr pos2 e1
        <|> lambdalike env Nothing
    exprLeftList = do
        ((pos0, op, pos1, e1):os) <- parseOps $ leftops env
        return $ foldl (eleftop pos0) (EUnary pos0 op `EApp` ArgExpr pos1 e1) os
    parseOps ops = do
        pos0 <- getPos
        op <- foldr (<|>) empty $ map (\ op -> keywordOp op *> return op) $ Map.keys ops
        pos1 <- getPos
        empty
            <|> do
                e <- exprApp
                os <- return [] <|> parseOps ops
                return ((pos0, op, pos1, e):os)
            <|> do
                e <- lambdalike env Nothing
                return [(pos0, op, pos1, e)]
    eleftop pos0 e0 (posop, op, pose, e1) = EVar posop op `EApp` ArgExpr pos0 e0 `EApp` ArgExpr pose e1
    erightop (posop0, op0, pose0, e0) (posop1, op1, pose1, e1) =
        (posop0, op0, pose0, EVar posop1 op1 `EApp` ArgExpr pose0 e0 `EApp` ArgExpr pose1 e1)
    exprApp = foldl EApp <$> exprFun <*> many exprArg
    exprFun = empty
        <|> exprAtom
        <|> special env
    exprAtom = empty
        <|> parens (expr env)
        <|> parens (EVar <$> getPos <*> operator env)
        <|> EVar <$> getPos <*> var env
        <|> ENumber <$> getPos <*> decimal
        <|> do
            pos0 <- getPos
            (v, s) <- lexeme $ do
                v <- (:) <$> idStartChar <*> many idContChar
                s <- char '{' *> quoteItems env [] <* char '}'
                return (v, s)
            return $ EQLO pos0 v s
        <|> uncurry EGens <$> angleBrackets ((,) <$> generators env <*> getPos)
    exprArg = empty
        <|> ArgExpr <$> getPos <*> exprAtom
        <|> ArgField <$> getPos <*> (keywordOp "#" *> field)

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
        <|> parens (pattern env)
        <|> PView <$> getPos <*> ident
        <|> PVar <$> getPos <*> (lexeme (char '\'') *> ident)
        <|> PDiscard <$> (getPos <* underscoreTerm)

generators :: Env -> Parser Char [(Pos, Generator)]
generators env = ((,) <$> getPos <*> generator env) `endBy` semicolon

generator :: Env -> Parser Char Generator
generator env = empty
    <|> MatchGenerator <$> boundName <*> getPos <*> (keywordOp "∝" *> expr env)
    <|> ExecGenerator <$> getPos <*> (underscoreTerm *> keywordOp "←" *> expr env)
    <|> BindGenerator <$> (lexeme (char '\'') *> ident) <*> getPos <*> (keywordOp "←" *> expr env)

data SourceComp
  = SCChar Char
  | SCPos Pos
  | SCImports
  | SCDeclareVar String String
  | SCDeclareView String String
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
  | EGens [(Pos, Generator)] Pos
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
  = MatchGenerator String Pos Expr
  | ExecGenerator Pos Expr
  | BindGenerator String Pos Expr

data Param
  = HSVSParam [String]
  | FVSParam [String]

var :: Env -> Parser Char String
var env = lexeme $ do
    v <- identName
    notFollowedBy (char '{')
    case Map.member v (lambdas env) || Map.member v (specials env) of
        False -> return v
        True -> pfail $ v ++ " is not variable-like"

operator :: Env -> Parser Char String
operator env = lexeme $ do
    v <- operatorComp
    return v

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

special :: Env -> Parser Char Expr
special env = do
    pos <- getPos
    foldr (<|>) empty $
        map (\ (v, f) -> foldl EApp <$> (EVar <$> getPos <*> (keyword v *> return v)) <*> f env) $
            Map.toList (specials env)

field :: Parser Char String
field = lexeme $ name

name :: Parser Char String
name = nameOf alphaNumComp <|> nameOf numComp

boundName :: Parser Char String
boundName = lexeme (char '\'') *> lexeme (nameOf alphaNumComp <|> nameOf numComp)

ident :: Parser Char String
ident = lexeme identName

identName :: Parser Char String
identName = nameOf alphaNumComp

nameOf :: Parser Char String -> Parser Char String
nameOf init = (++) <$> init <*> (concat <$> many cont <* notFollowedBy cont) where
    cont =
            (:) <$> matching "separator" (`elem` "-.") <*> alphaNumComp
        <|> (:) <$> matching "separator" (`elem` ".") <*> operatorComp

numComp :: Parser Char String
numComp = ((:) <$> digit <*> many digit) <* notFollowedBy digit

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

operatorComp :: Parser Char String
operatorComp = ((:) <$> opStartChar <*> many opContChar) <* notFollowedBy opContChar

opStartChar :: Parser Char Char
opStartChar = matching "operator start character" (\ c -> isSymbol c || (isPunctuation c && not (c `elem` "()[]{}〈〉《》`_,;.'\"\\")))

opContChar :: Parser Char Char
opContChar = matching "operator continuation character" isSymbol

parens :: Parser Char a -> Parser Char a
parens p = lexeme (char '(') *> p <* lexeme (char ')')

angleBrackets :: Parser Char a -> Parser Char a
angleBrackets p = lexeme (char '〈' *> notFollowedBy (char '{')) *> p <* lexeme (char '〉')

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
            empty
                <|> EMissingCase <$> getPos
                <|> expr env,
            Nothing
        )),
        ("analyze-type-checker-M", \ env -> (
            expr env,
            empty
                <|> EMissingCase <$> getPos
                <|> expr env,
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
        ("λ", \ env -> (EPat <$> pattern env, EOpen <$> expr env, Nothing)),
        ("for", \ env -> (EGens <$> generators env <*> getPos, EOpen <$> expr env, Nothing)),
        ("impfor", \ env -> (EImpGens <$> generators env <*> getPos, EOpen <$> expr env, Nothing)),
        ("either.for", \ env -> (EMonadGens <$> generators env <*> getPos, EOpen <$> expr env, Nothing)),
        ("parser.for", \ env -> (EMonadGens <$> generators env <*> getPos, EOpen <$> expr env, Nothing)),
        ("type-checker.for", \ env -> (EMonadGens <$> generators env <*> getPos, EOpen <$> expr env, Nothing))
    ],
    specials = Map.fromList [
        ("value", \ env -> (:) <$> (ArgExpr <$> getPos <*> (EVar <$> getPos <*> lexeme identName)) <*> return [])
    ],
    leftops = Map.fromList [
        ("*", ()),
        ("*>", ()),
        ("+", ()),
        ("-", ()),
        ("-?", ()),
        ("<*", ()),
        ("<*>", ()),
        ("<|>", ()),
        ("||", ())
    ],
    rightops = Map.fromList [
        ("$", ()),
        (":", ()),
        ("<>", ())
    ],
    nonops = Map.fromList [
        ("≠", ())
    ]
  }

data Env = Env {
    lambdas :: Map String (Env -> (Parser Char Expr, Parser Char Expr, Maybe (Parser Char Expr))),
    specials :: Map String (Env -> Parser Char [Arg]),
    leftops :: Map String (),
    rightops :: Map String (),
    nonops :: Map String ()
  }

hsident :: Parser Char String
hsident = hslexeme ((:) <$> hsidStartChar <*> many hsidContChar) <* notFollowedBy hsidContChar

hsidStartChar :: Parser Char Char
hsidStartChar = matching "identifier" isAlpha

hsidContChar :: Parser Char Char
hsidContChar = matching "identifier continuation character" (\ ch -> isAlphaNum ch || ch == '_')

hslexeme :: Parser Char a -> Parser Char a
hslexeme p = p <* hswhitespace

hswhitespace :: Parser Char ()
hswhitespace = empty
    <|> return ()
    <|> matching "whitespace" isSpace *> hswhitespace

scCode :: SourceComp -> String
scCode SCChar{} = "SCChar"
scCode SCPos{} = "SCPos"
scCode SCImports{} = "SCImports"
scCode SCDeclareVar{} = "SCDeclareVar"
scCode SCDeclareView{} = "SCDeclareView"
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
eCode EGens{} = "EGens"
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
genCode MatchGenerator{} = "MatchGenerator"
genCode ExecGenerator{} = "ExecGenerator"
genCode BindGenerator{} = "BindGenerator"
