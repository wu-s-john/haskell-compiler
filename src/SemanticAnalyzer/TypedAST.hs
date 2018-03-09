{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.TypedAST where

import Control.Monad (unless, when)
import Control.Monad.Writer (Writer, tell)
import Parser.AST as AST
import qualified Parser.TerminalNode as T

data SemanticError = NonIntArgumentsPlus
  { left :: T.Type
  , right :: T.Type
  } deriving (Show, Eq)

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  deriving (Show, Eq)

computeType :: ExpressionT -> T.Type
computeType (IntegerExprT _) = "Int"
computeType (PlusExprT _ _) = "Int"
computeType (StringExprT _) = "String"

semanticAnalyzer :: AST.Expression -> Writer [SemanticError] ExpressionT
semanticAnalyzer (AST.IntegerExpr value) = return (IntegerExprT value)
semanticAnalyzer (AST.StringExpr value) = return (StringExprT value)
semanticAnalyzer (AST.PlusExpr left' right') = do
  annotatedLeft <- semanticAnalyzer left'
  annotatedRight <- semanticAnalyzer right'
  let leftType = computeType annotatedLeft
  let rightType = computeType annotatedRight
  unless
    (computeType annotatedLeft == "Int" && computeType annotatedRight == "Int")
    (tell [NonIntArgumentsPlus leftType rightType])
  return $ PlusExprT annotatedLeft annotatedRight

