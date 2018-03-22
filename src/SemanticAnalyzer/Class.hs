{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.Class where

import qualified Data.Map as M
import Data.Maybe (fromJust, mapMaybe)
import Data.String (IsString, fromString)

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Type (Type(..))

import Parser.ParserUtil (parse)
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
                , attributes :: AttributeMap }
  | ObjectClass
  -- todo implement "defaultValue" when doing code translation
  deriving (Show, Eq)

getMethods :: ClassRecord -> MethodMap
getMethods (ClassRecord _ _ classMethods _) = classMethods
getMethods ObjectClass =
  M.fromList
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

instance IsString MethodRecord where
  fromString code = fromJust $ toRecord (parse $ code ++ "{8}") -- in the form of <methodName>(<parameters>) : <returnType>

instance FeatureTransformer AttributeRecord where
  toRecord attribute =
    case attribute of
      AST.Method {} -> Nothing
      AST.Attribute name typeName _ -> Just $ AttributeRecord name $ fromString typeName
  getName (AttributeRecord name _) = name

instance IsString AttributeRecord where
  fromString code = fromJust $ toRecord (parse code)

class Mappable a where
  toMap ::
       FeatureTransformer b
    => IsString b =>
         [a] -> M.Map T.Identifier b

instance Mappable AST.Feature where
  toMap features = M.fromList $ map (\methodRecord -> (getName methodRecord, methodRecord)) $ mapMaybe toRecord features

instance Mappable String where
  toMap codes = M.fromList $ map (\methodRecord -> (getName methodRecord, methodRecord)) features
    where
      features = map fromString codes
