{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.InheritanceFeatureError where

import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Type (Type)

data InheritanceFeatureError
  = RedefinedAttribute { attributeName :: Identifier
                       , inheritedParent :: Type }
  | DifferentMethodReturnType { newType :: Type
                              , originalType :: Type }
  deriving (Show, Eq)
