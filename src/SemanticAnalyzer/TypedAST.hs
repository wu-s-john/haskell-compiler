{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.TypedAST where

import Control.Monad (unless, when)
import Control.Monad.State (State,get)
import Control.Monad.Writer (Writer, WriterT, tell)
import qualified Data.Map as M
import Parser.AST as AST
import qualified Parser.TerminalNode as T

data SemanticError
  = NonIntArgumentsPlus { left :: T.Type
                        , right :: T.Type }
  | UndeclaredIdentifier T.Identifier
  deriving (Show, Eq)

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  | IdentifierExprT { name :: T.Identifier
                    , typeName :: T.Type }
  deriving (Show, Eq)

computeType :: ExpressionT -> T.Type
computeType (IntegerExprT _) = "Int"
computeType (PlusExprT _ _) = "Int"
computeType (StringExprT _) = "String"
computeType (IdentifierExprT _ typeName) = typeName

type ObjectEnvironment = M.Map T.Identifier T.Type

type SemanticAnalyzer = WriterT [SemanticError] (State ObjectEnvironment)

semanticCheck :: AST.Expression -> SemanticAnalyzer ExpressionT
semanticCheck (AST.IntegerExpr value) = return (IntegerExprT value)
semanticCheck (AST.StringExpr value) = return (StringExprT value)
semanticCheck (AST.PlusExpr left' right') = do
  annotatedLeft <- semanticCheck left'
  annotatedRight <- semanticCheck right'
  let leftType = computeType annotatedLeft
  let rightType = computeType annotatedRight
  unless
    (computeType annotatedLeft == "Int" && computeType annotatedRight == "Int")
    (tell [NonIntArgumentsPlus leftType rightType])
  return $ PlusExprT annotatedLeft annotatedRight
semanticCheck (AST.IdentifierExpr identifierName) = do
  objectIdentifier <- get
  case identifierName `M.lookup` objectIdentifier of
    Nothing -> tell [UndeclaredIdentifier identifierName] >> return (IdentifierExprT identifierName "Object")
    Just typeName' -> return (IdentifierExprT identifierName typeName')
