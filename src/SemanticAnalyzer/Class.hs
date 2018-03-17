{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.Class where

import qualified Data.Map as M

import Data.Maybe (mapMaybe)

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T

data MethodRecord = MethodRecord
  { methodName :: T.Identifier
  , parameters :: [(T.Identifier, T.Type)]
  , returnType :: T.Type
  } deriving (Show, Eq)

data AttributeRecord = AttributeRecord
  { attributeName :: T.Identifier
  , typeName :: T.Type
  } deriving (Show, Eq)

type MethodMap = M.Map T.Identifier MethodRecord

type AttributeMap = M.Map T.Identifier AttributeRecord

data ClassRecord
  = ClassRecord { className :: T.Type
                , parent :: ClassRecord
                , methods :: MethodMap -- contains methods of a class
                , attributes :: AttributeMap -- contains attributes of a class
                 }
  | ObjectClass
  | BasicClass { className :: T.Type
               , methods :: MethodMap }
  -- todo implement "defaultValue" when doing default value
  -- todo include IO which only has a class name and methods
  deriving (Show, Eq)

class FeatureTransformer a where
  toRecord :: AST.Feature -> Maybe a
  getName :: a -> String

instance FeatureTransformer MethodRecord where
  toRecord feature =
    case feature of
      AST.Attribute {} -> Nothing
      AST.Method name formalParameters' typeName' _ ->
        Just $ MethodRecord name (map convertFormalToTuple formalParameters') typeName'
    where
      convertFormalToTuple (AST.Formal identifierName typeName') = (identifierName, typeName')
  getName (MethodRecord name _ _) = name

instance FeatureTransformer AttributeRecord where
  toRecord attribute =
    case attribute of
      AST.Method {} -> Nothing
      AST.Attribute name type' _ -> Just $ AttributeRecord name type'
  getName (AttributeRecord name _) = name

--
toMap :: FeatureTransformer a => [AST.Feature] -> M.Map T.Identifier a
toMap features = M.fromList $ map (\methodRecord -> (getName methodRecord, methodRecord)) $ mapMaybe toRecord features
