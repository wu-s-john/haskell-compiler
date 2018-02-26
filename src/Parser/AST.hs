{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Parser.AST where

import Parser.TerminalNode

newtype Program = Program
  { getClasses :: [Class]
  } deriving (Show, Read, Eq)

data Class
  = OrphanedClass { getClassName :: Type
                  , getFeatures :: [Feature] }
  | InheritedClass { getClassName :: Type
                   , getInheritName :: Type
                   , getFeatures :: [Feature] }
  deriving (Show, Read, Eq)

data Feature
  = Attribute { getName :: Identifier
              , getType :: Type
              , getInitExpr :: Maybe Expression }
  | Method { getName :: Identifier
           , getFormals :: [Formal]
           , getType :: Type
           , getExpr :: Expression }
  deriving (Show, Read, Eq)

data Formal = Formal
  { getIdentifier :: Identifier
  , getType :: Type
  } deriving (Show, Read, Eq)

data Expression
  = AssignmentExpr { getIdentifier :: Identifier
                   , getRight :: Expression }
  | MethodDispatch { getExpr :: Expression
                   , getMethodName :: Identifier
                   , getParameters :: [Expression] }
  | StaticMethodDispatch { getExpr :: Expression
                         , getParentType :: Type
                         , getMethodName :: Identifier
                         , getParameters :: [Expression] }
  | CondExpr { getPred :: Expression
             , getThenBranch :: Expression
             , getElseBranch :: Expression }
  | LoopExpr { getPred :: Expression
             , getBody :: Expression }
  | BlockExpr [Expression]
  | LetExpr LetBinding
  | TypeCaseExpr { getExpr :: Expression
                 , getBranches :: [CaseBranch] }
  | NewExpr { getType :: Type }
  | IsvoidExpr { getExpr :: Expression }
  | PlusExpr { getLeft :: Expression
             , getRight :: Expression }
  | MinusExpr { getLeft :: Expression
              , getRight :: Expression }
  | TimesExpr { getLeft :: Expression
              , getRight :: Expression }
  | DivideExpr { getLeft :: Expression
               , getRight :: Expression }
  | NegExpr { getExpr :: Expression }
  | LessThanExpr { getLeft :: Expression
                 , getRight :: Expression }
  | LessThanOrEqualExpr { getLeft :: Expression
                        , getRight :: Expression }
  | EqualExpr { getLeft :: Expression
              , getRight :: Expression }
  | NotExpr { getExpr :: Expression }
  | SelfVarExpr
  | IdentifierExpr String
  | IntegerExpr Int
  | StringExpr String
  | TrueExpr
  | FalseExpr
  deriving (Show, Read, Eq)

data CaseBranch = CaseBranch
  { getIdentifier :: Identifier
  , getType :: Type
  , getExpr :: Expression
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
