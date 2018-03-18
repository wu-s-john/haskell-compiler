{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassEnvironment where

import Control.Monad (join)
import Control.Monad.Writer (Writer, tell)
import qualified Data.Map as M
import Parser.AST (Class(..), Program(..))
import SemanticAnalyzer.Class
       (AttributeMap, ClassRecord(..), MethodMap, MethodRecord(..), toMap)
import qualified SemanticAnalyzer.ClassChecker as CC
import SemanticAnalyzer.Type (Type(..))
import qualified Parser.TerminalNode as T

type ClassEnvironment = M.Map String ClassRecord

data InheritanceErrors
  = RedefinedAttribute { attributeName :: T.Identifier
                       , inheritedParent :: Type }
  | DifferentMethodReturnType { newType :: Type
                              , originalType :: Type }
  deriving (Show, Eq)

--todo this will probably be part of a class
createEnvironment :: CC.ClassInheritanceGraph -> Program -> ClassEnvironment
createEnvironment inheritanceGraph (Program classes) = cache
  where
    cache = M.fromList $ map toClassTupleRecord classes
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
    toClassTupleRecord (Class currentClassName _ features) = (currentClassName, computeClassRecord currentClassName features)

mergeAttributes :: Type -> AttributeMap -> AttributeMap -> Writer [InheritanceErrors] AttributeMap
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
