{-# OPTIONS_GHC -Wall #-}

module Parser.AST where

import Lexer.Token


data BinaryOp =
  PlusExp

data Expression =
  BinaryOp {getOp :: Token, getLeft :: Expression, getRight :: Expression}
  | UnaryOp {getOp :: Token, getExpr :: Expression}
  | IntegerExpr Int
  | IdentifierExpr String
  deriving (Show, Read, Eq)

newtype Identifier = Identifier String deriving (Show, Read, Eq)
newtype Type = Type String deriving (Show, Read, Eq)

data Feature = Attribute {getName :: Identifier, getType :: Type, getFeatExpr :: Maybe Expression} deriving (Show, Read, Eq)
