{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.SemanticCheckUtil
  ( (<==)
  , (\/)
  , (/>)
  , (<==?)
  , lookupClass'
  , coerceType
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map as M
import qualified Data.Set as S

import SemanticAnalyzer.Class (ClassRecord(..))
import SemanticAnalyzer.SemanticAnalyzer
       (SemanticAnalyzer, SemanticAnalyzerM, invokeClassName, lookupClass)

import Control.Monad.Extra (maybeM)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT))
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.PrimitiveTypes (primitiveTypes)
import SemanticAnalyzer.Type (Type(..))
import SemanticAnalyzer.TypedAST (ExpressionT(..), computeType)

class (Monad m) =>
      Categorical a m where
  (<==) :: a -> a -> m Bool -- determines if the left argument is a subset of the right
  (\/) :: a -> a -> m a -- determines the lub of two types

instance Categorical ClassRecord Identity where
  _ <== ObjectClass = return True
  (ClassRecord possibleSubtypeName possibleParentRecord _ _) <== parentRecord@(ClassRecord superClassName _ _ _)
    | possibleSubtypeName == superClassName = return True
    | superClassName `elem` primitiveTypes = return False
    | otherwise = possibleParentRecord <== parentRecord
  ObjectClass <== _ = return False
  ObjectClass \/ _ = return ObjectClass
  _ \/ ObjectClass = return ObjectClass
  leftClassRecord \/ rightClassRecord = do
    let rightClassAncestors = computeAncestors rightClassRecord
    return $ lub leftClassRecord rightClassAncestors
    where
      computeAncestors ObjectClass = S.fromList ["Object"]
      computeAncestors (ClassRecord className' parent' _ _) = className' `S.insert` computeAncestors parent'
      lub ObjectClass _ = ObjectClass
      lub classRecord@(ClassRecord className' parent' _ _) ancestors
        | className' `elem` ancestors = classRecord
        | otherwise = lub parent' ancestors

instance Categorical Type SemanticAnalyzer where
  (TypeName _) <== SELF_TYPE = return False
  possibleSubType <== parentType = do
    parentClassRecord <- getClassRecord parentType
    subclassRecord <- getClassRecord possibleSubType
    return (runIdentity $ subclassRecord <== parentClassRecord)
  SELF_TYPE \/ SELF_TYPE = return SELF_TYPE
  leftType \/ rightType = do
    leftClassRecord <- getClassRecord leftType
    rightClassRecord <- getClassRecord rightType
    case runIdentity $ leftClassRecord \/ rightClassRecord of
      (ClassRecord className' _ _ _) -> return (TypeName className')
      ObjectClass -> return (TypeName "Object")

class FindableClassRecord a where
  lookupClass' :: a -> MaybeT SemanticAnalyzer ClassRecord

instance FindableClassRecord String where
  lookupClass' = MaybeT . lookupClass

instance FindableClassRecord ExpressionT where
  lookupClass' expressionT =
    MaybeT $ do
      let typeVal' = computeType expressionT
      typeString <- toString typeVal'
      lookupClass typeString

instance FindableClassRecord (MaybeT SemanticAnalyzer ExpressionT) where
  lookupClass' maybeExpressionT = do
    expressionT <- maybeExpressionT
    lookupClass' expressionT

(/>) :: (Identifier, Type) -> SemanticAnalyzer a -> SemanticAnalyzer a -- temporarily adds a type to the object environment
(identifier', typeName') /> semanticAnalyzer = do
  objectEnvironment <- get
  put $ M.insert identifier' typeName' objectEnvironment
  result <- semanticAnalyzer
  put objectEnvironment
  return result

(<==?) :: SemanticAnalyzerM ClassRecord -> SemanticAnalyzerM ClassRecord -> SemanticAnalyzerM Bool
maybePossibleSubtypeClassRecord <==? maybeAncestorTypeClassRecord = do
  possibleSubtypeClassRecord <- maybePossibleSubtypeClassRecord
  ancestorTypeClassRecord <- maybeAncestorTypeClassRecord
  return $ runIdentity (ancestorTypeClassRecord <== possibleSubtypeClassRecord)

getClassRecord :: Type -> SemanticAnalyzer ClassRecord
getClassRecord (TypeName currentClassName)
  | currentClassName == "Object" = return ObjectClass
  | otherwise =
    maybeM (return (ClassRecord currentClassName ObjectClass M.empty M.empty)) return (lookupClass currentClassName)
getClassRecord SELF_TYPE = do
  (typeName, _) <- ask
  getClassRecord (TypeName typeName)

coerceType :: Type -> SemanticAnalyzer Type
coerceType SELF_TYPE = invokeClassName $ \typeName' -> return $ TypeName typeName'
coerceType type'@(TypeName _) = return type'

toString :: Type -> SemanticAnalyzer String
toString type' = do
  (TypeName typeString) <- coerceType type'
  return typeString
