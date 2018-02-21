{-# OPTIONS_GHC -Wall #-}

module Parser.AST where

import Lexer.Token
import Parser.TerminalNode


data Expression =
    BinaryOp {getBinaryOp :: BinaryOpTerminal, getLeft :: Expression, getRight :: Expression}
  | UnaryOp {getUnaryOp :: Token, getExpr :: Expression}
  | IntegerExpr Int
  | IdentifierExpr String
  | BlockExpression [Expression]
  | ExpressionError
  deriving (Show, Read, Eq)

data Class =
    OrphanedClass {getClassName :: Type, getFeatures :: [Feature]}
  | InheritedClass {getClassName :: Type, getInheritName :: Type, getFeatures :: [Feature]}
  deriving (Show, Read, Eq)

newtype Identifier = Identifier String deriving (Show, Read, Eq)
newtype Type = Type String deriving (Show, Read, Eq)

data Feature = Attribute {getName :: Identifier, getType :: Type, getFeatExpr :: Maybe Expression} deriving (Show, Read, Eq)
