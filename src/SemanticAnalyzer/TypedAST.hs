{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.TypedAST where

import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Type

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  | IdentifierExprT { name :: T.Identifier
                    , typeVal :: Type }
  | LetExprT LetBindingT
  | MethodDispatchT { expr :: ExpressionT
                    , methodName :: T.Identifier
                    , parameters :: [ExpressionT]
                    , typeVal :: Type }
  | StaticMethodDispatchT { expr :: ExpressionT
                           , parentTypeName :: String
                           , methodName :: T.Identifier
                           , parameters :: [ExpressionT]
                           , typeVal :: Type}
  | SelfVarExprT
  | NewExprT { className :: String
             , typeVal :: Type }
  deriving (Show, Eq)

data LetBindingT
  = LetBindingT { getIdentifier :: T.Identifier
                , getType :: Type
                , getInitExpr :: Maybe ExpressionT
                , getExpr :: ExpressionT }
  | LetDeclarationT { getIdentifier :: T.Identifier
                    , getType :: Type
                    , getInitExpr :: Maybe ExpressionT
                    , getLetBinding :: LetBindingT }
  deriving (Show, Eq)

computeType :: ExpressionT -> Type
computeType (IntegerExprT _) = TypeName "Int"
computeType (PlusExprT _ _) = TypeName "Int"
computeType (StringExprT _) = TypeName "String"
computeType (IdentifierExprT _ classType) = classType
computeType SelfVarExprT = SELF_TYPE
