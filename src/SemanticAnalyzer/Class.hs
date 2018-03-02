{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.Class where

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T

data MethodRecord = MethodRecord
  { getName :: T.Identifier
  , getParameters :: [AST.Formal]
  , getReturnType :: T.Type
  } deriving (Show, Eq)

data AttributeRecord = AttributeRecord
  { getName :: T.Identifier
  , getType :: T.Type
  } deriving (Show, Eq)

data ClassRecord = ClassRecord
  { getName :: T.Identifier
  , getParent :: ClassRecord
  , getMethods :: [MethodRecord] -- deal with Attributes
  } | ObjectClass 

extractMethodRecord :: AST.Feature -> Maybe MethodRecord
extractMethodRecord feature =
  case feature of
    AST.Attribute {} -> Nothing
    AST.Method name parameters typeName _ -> Just $ MethodRecord name parameters typeName

extractAttributeRecord :: AST.Feature -> Maybe AttributeRecord
extractAttributeRecord feature =
  case feature of
    AST.Method {} -> Nothing
    AST.Attribute name type' _ -> Just $ AttributeRecord name type'
