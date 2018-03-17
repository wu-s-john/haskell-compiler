{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.SemanticCheckUtil
  ( (<==)
  , (\/)
  , (/>)
  ) where

import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Map as M
import qualified Data.Set as S
import SemanticAnalyzer.Class (ClassRecord(..))
import SemanticAnalyzer.SemanticAnalyzer (SemanticAnalyzer)

import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.PrimitiveTypes (primitiveTypes)

class (Monad m) =>
      Categorical a m where
  (<==) :: a -> a -> m Bool -- determines if the left argument is a subset of the right
  (\/) :: a -> a -> m a -- determines the lub of two types

(/>) :: (T.Identifier, T.Type) -> SemanticAnalyzer a -> SemanticAnalyzer a -- temporarily adds a type to the object environment
(identifier', typeName') /> semanticAnalyzer = do
  objectEnvironment <- get
  put $ M.insert identifier' typeName' objectEnvironment
  result <- semanticAnalyzer
  put objectEnvironment
  return result

instance Categorical ClassRecord Identity where
  _ <== ObjectClass = return True
  (ClassRecord possibleSubtypeName possibleParentRecord _ _) <== parentRecord@(ClassRecord parentTypeName _ _ _)
    | possibleSubtypeName == parentTypeName = return True
    | parentTypeName `elem` primitiveTypes = return False
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

instance Categorical T.Type SemanticAnalyzer where
  possibleSubType <== parentType = do
    parentClassRecord <- getClassRecord parentType
    subclassRecord <- getClassRecord possibleSubType
    return (runIdentity $ subclassRecord <== parentClassRecord)
  leftType \/ rightType = do
    leftClassRecord <- getClassRecord leftType
    rightClassRecord <- getClassRecord rightType
    case runIdentity $ leftClassRecord \/ rightClassRecord of
      (ClassRecord className' _ _ _) -> return className'
      ObjectClass -> return "Object"

getClassRecord :: T.Type -> SemanticAnalyzer ClassRecord
getClassRecord evaluatingType = do
  (_, classEnvironment) <- ask
  if | evaluatingType == "Object" -> return ObjectClass
     | otherwise ->
       case evaluatingType `M.lookup` classEnvironment of
         Just classRecord -> return classRecord
         Nothing -> return (ClassRecord evaluatingType ObjectClass M.empty M.empty)
