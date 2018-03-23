{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassRelationshipError where

data ClassRelationshipError
  = UndefinedInheritance String
                         String
  | PrimitiveInheritance String
                         String
  | InheritanceCycle String
  | PreviouslyDefined String
  deriving (Show, Eq)
