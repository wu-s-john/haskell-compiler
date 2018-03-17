{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.Class where

import qualified Data.Map as M

import Data.Maybe (mapMaybe)

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T

import Util

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
  -- todo implement "defaultValue" when doing translation
  deriving (Show, Eq)

getMethods :: ClassRecord -> MethodMap
getMethods (ClassRecord _ _ classMethods _) = classMethods
getMethods ObjectClass =
  [ "abort" =: MethodRecord "abort" [] "Object"
  , "type_name" =: MethodRecord "type_name" [] "String"
  , "copy" =: MethodRecord "copy" [] "SELF_TYPE"
  ]

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
