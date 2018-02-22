{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Parser.AST where

import Parser.TerminalNode

data Class
  = OrphanedClass { getClassName :: Type
                  , getFeatures :: [Feature] }
  | InheritedClass { getClassName :: Type
                   , getInheritName :: Type
                   , getFeatures :: [Feature] }
  deriving (Show, Read, Eq)

data Feature = Attribute
  { getName :: Identifier
  , getType :: Type
  , getExpr :: Maybe Expression
  } deriving (Show, Read, Eq)

data LetBinding
  = LetBinding { getIdentifier :: Identifier
               , getType :: Type
               , getInitExpr :: Maybe Expression
               , getExpr :: Expression }
  | LetDeclaration { getIdentifier :: Identifier
                   , getType :: Type
                   , getInitExpr :: Maybe Expression
                   , getLetBinding :: LetBinding }
  deriving (Show, Read, Eq)

data CaseBranch = CaseBranch
  { getIdentifier :: Identifier
  , getType :: Type
  , getExpr :: Expression
  } deriving (Show, Read, Eq)

data Expression
  = BinaryOp { getBinaryOp :: BinaryOpTerminal
             , getLeft :: Expression
             , getRight :: Expression }
  | UnaryOp { getUnaryOp :: UnaryOpTerminal
            , getExpr :: Expression }
  | IntegerExpr Int
  | IdentifierExpr String
  | StringExpr String
  | BlockExpression [Expression]
  | AssignmentExpression { getLeft :: Expression
                         , getRight :: Expression }
  | NewExpression { getType :: Type }
  | LetExpression LetBinding
  | TypeCaseExpression Expression [CaseBranch]
  deriving (Show, Read, Eq)
