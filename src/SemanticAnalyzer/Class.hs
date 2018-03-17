{-# OPTIONS_GHC -Wall -Wcompat #-}
{-# LANGUAGE DuplicateRecordFields #-}

module SemanticAnalyzer.Class where

import qualified Data.Map as M

import Control.Monad (join)
import Control.Monad.Writer (Writer, tell)
import Data.Maybe (mapMaybe)

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

type ClassEnvironment = M.Map String ClassRecord

--todo this will probably be part of a class
createEnvironment :: CC.ClassInheritanceGraph -> AST.Program -> ClassEnvironment
createEnvironment inheritanceGraph (AST.Program classes) = cache
  where
    cache =
      M.fromList [(className', computeClassRecord className' features) | (AST.Class className' _ features) <- classes]
    computeClassRecord className' features =
      let newAttributes = toMap features
          newMethods = toMap features
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

--
toMap :: FeatureTransformer a => [AST.Feature] -> M.Map T.Identifier a
toMap features = M.fromList $ map (\methodRecord -> (getName methodRecord, methodRecord)) $ mapMaybe toRecord features
