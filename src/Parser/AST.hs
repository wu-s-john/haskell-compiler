{-# OPTIONS_GHC -Wall #-}

module Parser.AST where

import Lexer.Token


data BinaryOp =
  PlusExp

data Expression =
  BinaryOp {getOp :: Token, getLeft :: Expression, getRight :: Expression}
  | UnaryOp {getOp :: Token, getExpr :: Expression}
  | IntegerExpr Int
  deriving (Show, Read, Eq)

