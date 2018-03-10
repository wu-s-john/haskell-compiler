{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DeriveGeneric #-}

module SemanticAnalyzer.Class where

import Data.Aeson
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import GHC.Generics

import Control.Monad (join)
import Control.Monad.Writer (Writer, tell)
import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T
import qualified SemanticAnalyzer.ClassChecker as CC

data InheritanceErrors
  = RedefinedAttribute { attributeName :: String
                       , inheritedParent :: T.Type }
  | DifferentMethodReturnType { newType :: T.Type
                              , originalType :: T.Type }
  deriving (Show, Eq)

data MethodRecord = MethodRecord
  { methodName :: T.Identifier
  , parameters :: [AST.Formal]
  , returnType :: T.Type
  } deriving (Show, Eq, Generic)

instance FromJSON MethodRecord

data AttributeRecord = AttributeRecord
  { attributeName :: T.Identifier
  , typeName :: T.Type
  } deriving (Show, Eq, Generic)

instance FromJSON AttributeRecord

type MethodMap = M.Map T.Identifier MethodRecord

type AttributeMap = M.Map T.Identifier AttributeRecord

data ClassRecord
  = ClassRecord { className :: T.Type
                , parent :: ClassRecord
                , methods :: MethodMap -- deal with Attributes
                , attributes :: AttributeMap -- deal with Attributes
                 }
  | ObjectClass
  | BasicClass { className :: T.Type, methods :: MethodMap } --todo include default values for code translation
  -- todo include IO which only has a class name and methods
  deriving (Show, Eq, Generic)

extractMethodRecord :: AST.Feature -> Maybe MethodRecord
extractMethodRecord feature =
  case feature of
    AST.Attribute {} -> Nothing
    AST.Method name parameters' typeName' _ -> Just $ MethodRecord name parameters' typeName'

extractAttributeRecord :: AST.Feature -> Maybe AttributeRecord
extractAttributeRecord feature =
  case feature of
    AST.Method {} -> Nothing
    AST.Attribute name type' _ -> Just $ AttributeRecord name type'

type ClassEnvironment = M.Map String ClassRecord

--todo this will probably be part of a class
createEnvironment :: CC.ClassInheritanceGraph -> AST.Program -> ClassEnvironment
createEnvironment inheritanceGraph (AST.Program classes) = cache
  where
    cache =
      M.fromList [(className', computeClassRecord className' features) | (AST.Class className' _ features) <- classes]
    computeClassRecord className' features =
      let newAttributes = extractAttributes features
          newMethods = extractMethods features
          parentName' = inheritanceGraph M.! className'
          parentRecord = cache M.! parentName'
      in if inheritanceGraph M.! className' == "Object"
           then ClassRecord className' ObjectClass newMethods newAttributes
           else ClassRecord
                  className'
                  parentRecord
                  (newMethods `M.union` methods parentRecord)
                  (newAttributes `M.union` attributes parentRecord)

mergeAttributes :: T.Type -> AttributeMap -> AttributeMap -> Writer [InheritanceErrors] AttributeMap
mergeAttributes className' classAttrs parentAttr = do
  let redefinedAttrs = classAttrs `M.intersection` parentAttr
  tell [RedefinedAttribute attributeName' className' | attributeName' <- M.keys redefinedAttrs]
  return $ classAttrs `M.union` parentAttr

mergeMethods :: MethodMap -> MethodMap -> Writer [InheritanceErrors] MethodMap
mergeMethods classMethods parentMethods = do
  let redefinedMethods = classMethods `M.intersection` parentMethods
  tell $ join [evaluateError redefinedMethodNames | redefinedMethodNames <- M.keys redefinedMethods]
  return $ classMethods `M.union` parentMethods
  where
    evaluateError methodName' =
      let classMethod = classMethods M.! methodName'
          parentMethod = parentMethods M.! methodName'
          classMethodReturnType = returnType classMethod
          parentMethodReturnType = returnType parentMethod
      in [ DifferentMethodReturnType classMethodReturnType parentMethodReturnType
         | classMethodReturnType /= parentMethodReturnType
         ]

extractMethods :: [AST.Feature] -> MethodMap
extractMethods features =
  M.fromList $
  map (\methodRecord@(MethodRecord name _ _) -> (name, methodRecord)) $ mapMaybe extractMethodRecord features

extractAttributes :: [AST.Feature] -> AttributeMap
extractAttributes features =
  M.fromList $
  map (\attributeRecord@(AttributeRecord name _) -> (name, attributeRecord)) (mapMaybe extractAttributeRecord features)
