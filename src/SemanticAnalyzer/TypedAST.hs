{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.TypedAST where

import qualified Parser.TerminalNode as T

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  | IdentifierExprT { name :: T.Identifier
                    , typeName :: T.Type }
  | LetExprT LetBindingT
  | MethodDispatchT { expr :: ExpressionT
                    , methodName :: T.Identifier
                    , parameters :: [ExpressionT]
                    , typeName :: T.Type }
  | SelfVarExprT
  deriving (Show, Eq)

data LetBindingT
  = LetBindingT { getIdentifier :: T.Identifier
                , getType :: T.Type
                , getInitExpr :: Maybe ExpressionT
                , getExpr :: ExpressionT }
  | LetDeclarationT { getIdentifier :: T.Identifier
                    , getType :: T.Type
                    , getInitExpr :: Maybe ExpressionT
                    , getLetBinding :: LetBindingT }
  deriving (Show, Eq)

computeType :: ExpressionT -> T.Type
computeType (IntegerExprT _) = "Int"
computeType (PlusExprT _ _) = "Int"
computeType (StringExprT _) = "String"
computeType (IdentifierExprT _ typeName') = typeName'
computeType SelfVarExprT = "self"
