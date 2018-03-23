{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassEnvironment where

import Control.Monad (join)
import Control.Monad.Writer.Lazy (Writer, tell)
import qualified Data.Map.Lazy as M
import Data.String (fromString)
import Parser.AST (Class(..), Program(..))
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (AttributeMap, ClassRecord(..), MethodMap, MethodRecord(..), toMap)
import qualified SemanticAnalyzer.ClassChecker as CC
import SemanticAnalyzer.Type (Type(..))

type ClassEnvironment = M.Map String ClassRecord

data InheritanceErrors
  = RedefinedAttribute { attributeName :: T.Identifier
                       , inheritedParent :: Type }
  | DifferentMethodReturnType { newType :: Type
                              , originalType :: Type }
  deriving (Show, Eq)

-- todo Lazy evaluation cannot be implemented correctly for the time being with a Writer monad
createEnvironment :: CC.ClassInheritanceGraph -> Program -> Writer [InheritanceErrors] ClassEnvironment
createEnvironment inheritanceGraph (Program classes) = cacheM
  where
    cacheM = M.fromList <$> mapM toClassTupleRecord classes
    computeClassRecord className' features =
      let newAttributes = toMap features
          newMethods = toMap features
          parentName' = inheritanceGraph M.! className'
      in do parentRecord <-
              if parentName' == "Object"
                then return ObjectClass
                else (M.! parentName') <$> cacheM
            case parentRecord of
              (ClassRecord _ _ parentMethods parentAttributes) -> do
                attributes' <- mergeAttributes (fromString className') newAttributes parentAttributes
                methods' <- mergeMethods newMethods parentMethods
                return $ ClassRecord className' parentRecord methods' attributes'
              ObjectClass -> return $ ClassRecord className' ObjectClass newMethods newAttributes
    toClassTupleRecord (Class currentClassName _ features) = do
      computedClass <- computeClassRecord currentClassName features
      return (currentClassName, computedClass)

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
