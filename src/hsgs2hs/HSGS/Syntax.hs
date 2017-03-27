module HSGS.Syntax (SourceComp(..), Expr(..), Param(..), interpolation, scCode) where

import Control.Applicative (Alternative(..))

import HSGS.Parser (Parser)

interpolation :: Parser Char SourceComp
interpolation = empty

data SourceComp
  = SCChar Char
  | SCArg [Param] Expr

data Expr
  = EVar String

data Param
  = FVSParam [String]

scCode :: SourceComp -> String
scCode SCChar{} = "SCChar"
scCode SCArg{} = "SCArg"