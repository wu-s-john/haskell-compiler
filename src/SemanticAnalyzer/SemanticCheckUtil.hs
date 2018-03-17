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

class (Monad m) =>
      Categorical a m where
  (<==) :: a -> a -> m Bool -- determines if the left argument is a subset of the right
  (\/) :: a -> a -> m a -- determines the lub of two types

-- invariant classes can inherit from basic classes even though some basic classes cannot be instantiated
instance Categorical ClassRecord Identity where
  (ClassRecord possibleSubtype possibleParentRecord _ _) <== parentRecord@(ClassRecord parentType _ _ _)
    | possibleSubtype == parentType = return True
    | otherwise = possibleParentRecord <== parentRecord
  BasicClass {} <== ClassRecord {} = return False
  (ClassRecord possibleSubtype possibleParentRecord _ _) <== parentRecord@(BasicClass parentType _)
    | possibleSubtype == parentType = return True
    | otherwise = possibleParentRecord <== parentRecord
  ObjectClass <== ObjectClass = return True
  ObjectClass <== _ = return False
  _ <== ObjectClass = return True
  (BasicClass possibleSubtype _) <== (BasicClass parentType _) = return $ possibleSubtype == parentType
  ObjectClass \/ _ = return ObjectClass
  _ \/ ObjectClass = return ObjectClass
  leftClassRecord \/ rightClassRecord = do
    let rightClassAncestors = computeAncestors rightClassRecord
    return $ lub leftClassRecord rightClassAncestors
    where
      computeAncestors ObjectClass = S.fromList ["Object"] -- invariant all classes should hold "Object" except for String and Int
      computeAncestors (BasicClass className' _) = S.fromList [className', "Object"]
      computeAncestors (ClassRecord className' parent' _ _) = className' `S.insert` computeAncestors parent'
      lub ObjectClass _ = ObjectClass
      lub classRecord@(ClassRecord className' parent' _ _) ancestors
        | className' `elem` ancestors = classRecord
        | otherwise = lub parent' ancestors
      lub basicClass@(BasicClass className' _) ancestors
        | className' `elem` ancestors = basicClass
        | otherwise = ObjectClass

(/>) :: (T.Identifier, T.Type) -> SemanticAnalyzer a -> SemanticAnalyzer a -- temporarily adds a type to the object environment
(identifier', typeName') /> semanticAnalyzer = do
  objectEnvironment <- get
  put $ M.insert identifier' typeName' objectEnvironment
  result <- semanticAnalyzer
  put objectEnvironment
  return result

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
      (BasicClass className' _) -> return className'

getClassRecord :: T.Type -> SemanticAnalyzer ClassRecord
getClassRecord typeName' = do
  (_, classEnvironment) <- ask
  if | typeName' == "Object" -> return ObjectClass
     | otherwise ->
       case typeName' `M.lookup` classEnvironment of
         Just classRecord -> return classRecord
         Nothing -> return (ClassRecord typeName' ObjectClass M.empty M.empty)
     -- | parentType `elem` primitiveTypes -> (ClassRecord parentType [] []) todo would include primitivetypes later
