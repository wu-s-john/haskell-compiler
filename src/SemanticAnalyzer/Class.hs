{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.Class where

import qualified Data.Map as M
import Data.String (fromString)
import Data.Maybe (mapMaybe)

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Type (Type(..))

import Util

data MethodRecord = MethodRecord
  { methodName :: T.Identifier
  , parameters :: [(T.Identifier, Type)]
  , returnType :: Type
  } deriving (Show, Eq)

data AttributeRecord = AttributeRecord
  { attributeName :: T.Identifier
  , returnType :: Type
  } deriving (Show, Eq)

type MethodMap = M.Map T.Identifier MethodRecord

type AttributeMap = M.Map T.Identifier AttributeRecord

data ClassRecord
  = ClassRecord { className :: String
                , parent :: ClassRecord
                , methods :: MethodMap
                , attributes :: AttributeMap
                 }
  | ObjectClass
  -- todo implement "defaultValue" when doing code translation
  deriving (Show, Eq)

getMethods :: ClassRecord -> MethodMap
getMethods (ClassRecord _ _ classMethods _) = classMethods
getMethods ObjectClass = M.fromList
  [ "abort" =: MethodRecord "abort" [] (TypeName "Object")
  , "type_name" =: MethodRecord "type_name" [] (TypeName "String")
  , "copy" =: MethodRecord "copy" [] SELF_TYPE
  ]

class FeatureTransformer a where
  toRecord :: AST.Feature -> Maybe a
  getName :: a -> String

instance FeatureTransformer MethodRecord where
  toRecord feature =
    case feature of
      AST.Attribute {} -> Nothing
      AST.Method name arguments typeName _ ->
        Just $ MethodRecord name (map convertFormalToTuple arguments) $ fromString typeName
    where
      convertFormalToTuple (AST.Formal identifierName typeName) = (identifierName, fromString typeName)
  getName (MethodRecord name _ _) = name

instance FeatureTransformer AttributeRecord where
  toRecord attribute =
    case attribute of
      AST.Method {} -> Nothing
      AST.Attribute name typeName _ -> Just $ AttributeRecord name $ fromString typeName
  getName (AttributeRecord name _) = name

--
toMap :: FeatureTransformer a => [AST.Feature] -> M.Map T.Identifier a
toMap features = M.fromList $ map (\methodRecord -> (getName methodRecord, methodRecord)) $ mapMaybe toRecord features
