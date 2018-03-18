{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.Type where

import Data.String (IsString, fromString)

data Type
  = TypeName String
  | SELF_TYPE
  deriving (Show, Eq, Ord)

instance IsString Type where
  fromString "SELF_TYPE" = SELF_TYPE
  fromString typeName = TypeName typeName

