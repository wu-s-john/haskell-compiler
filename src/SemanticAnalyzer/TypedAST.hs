{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module SemanticAnalyzer.TypedAST where

import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.Reader (Reader, ReaderT, ask, runReader)
import Control.Monad.State (State, get, put)
import Control.Monad.Writer (WriterT, tell)
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.AST as AST
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (ClassEnvironment(..), ClassRecord(..))

data SemanticError
  = NonIntArgumentsPlus { left :: T.Type
                        , right :: T.Type }
  | UndeclaredIdentifier T.Identifier
  | MismatchDeclarationType { inferredType :: T.Type
                            , declaredType :: T.Type }
  | UndefinedMethod {methodName :: T.Type}
  | DispatchUndefinedClass {className :: T.Type}
  deriving (Show, Eq)

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  | IdentifierExprT { name :: T.Identifier
                    , typeName :: T.Type }
  | LetExprT LetBindingT
  | MethodDispatchT { expr :: ExpressionT, methodName :: T.Identifier, parameters :: [ExpressionT], typeName :: T.Type}
  | SelfVarExprT
  deriving (Show, Eq)

data LetBindingT
  = LetBindingT { getIdentifier :: T.Identifier
                , getType :: T.Type
                , getInitExpr :: Maybe ExpressionT
                , getExpr :: ExpressionT }
  | LetDeclarationT { getIdentifier :: T.Identifier
                    , getType :: T.Type
                    , getInitExpr :: Maybe ExpressionT
                    , getLetBinding :: LetBindingT }
  deriving (Show, Eq)

computeType :: ExpressionT -> T.Type
computeType (IntegerExprT _) = "Int"
computeType (PlusExprT _ _) = "Int"
computeType (StringExprT _) = "String"
computeType (IdentifierExprT _ typeName') = typeName'
computeType SelfVarExprT = "self"

type ObjectEnvironment = M.Map T.Identifier T.Type
--type Semantic

type SemanticAnalyzer = ReaderT ClassEnvironment (WriterT [SemanticError] (State ObjectEnvironment))

(/>) :: (T.Identifier, T.Type) -> SemanticAnalyzer a -> SemanticAnalyzer a -- temporarily adds a type to the object environment
(identifier', typeName') /> semanticAnalyzer = do
  objectEnvironment <- get
  put $ M.insert identifier' typeName' objectEnvironment
  result <- semanticAnalyzer
  put objectEnvironment
  return result

semanticCheck :: AST.Expression -> SemanticAnalyzer ExpressionT
semanticCheck (AST.IntegerExpr value) = return (IntegerExprT value)
semanticCheck (AST.StringExpr value) = return (StringExprT value)

semanticCheck (AST.PlusExpr left' right') = do
  annotatedLeft <- semanticCheck left'
  annotatedRight <- semanticCheck right'
  let leftType = computeType annotatedLeft
  let rightType = computeType annotatedRight
  unless
    (computeType annotatedLeft == "Int" && computeType annotatedRight == "Int")
    (tell [NonIntArgumentsPlus leftType rightType])
  return $ PlusExprT annotatedLeft annotatedRight

semanticCheck (AST.IdentifierExpr identifierName) = do
  objectIdentifier <- get
  case identifierName `M.lookup` objectIdentifier of
    Nothing -> tell [UndeclaredIdentifier identifierName] >> return (IdentifierExprT identifierName "Object")
    Just typeName' -> return (IdentifierExprT identifierName typeName')

semanticCheck (AST.LetExpr (LetBinding identifier typeName' maybeInitialExpression evaluatingExpression)) = do
  classEnvironment <- ask --todo look for a way to refactor this more cleanly
  (identifier, typeName') /> do
    evaluatingExpressionT <- semanticCheck evaluatingExpression
    let transformResult initialMaybeExpression =
          return $ LetExprT $ LetBindingT identifier typeName' initialMaybeExpression evaluatingExpressionT
    case maybeInitialExpression of
      Just initialExpression -> do
        initialExpressionT <- semanticCheck initialExpression
        let initialExpressionTypeName = computeType initialExpressionT
        unless
          (runReader (initialExpressionTypeName <== typeName') classEnvironment)
          (tell [MismatchDeclarationType initialExpressionTypeName typeName'])
        transformResult (Just initialExpressionT)
      Nothing -> transformResult Nothing

semanticCheck AST.SelfVarExpr = return SelfVarExprT

semanticCheck (AST.MethodDispatch expression methodName' _) = do
  initialExpressionT <- semanticCheck expression
  classEnvironment <- ask
  let callerExprClassName = computeType initialExpressionT
  case M.lookup "Foo" classEnvironment of
    Just _ -> tell [UndefinedMethod methodName']
    Nothing -> tell [DispatchUndefinedClass callerExprClassName]
  return (MethodDispatchT initialExpressionT methodName' [] "Object")

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

instance Categorical T.Type (Reader ClassEnvironment) where
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

getClassRecord :: T.Type -> Reader ClassEnvironment ClassRecord
getClassRecord typeName' = do
  classEnvironment <- ask
  if | typeName' == "Object" -> return ObjectClass
     | otherwise ->
       case typeName' `M.lookup` classEnvironment of
         Just classRecord -> return classRecord
         Nothing -> return (ClassRecord typeName' ObjectClass M.empty M.empty)
     -- | parentType `elem` primitiveTypes -> (ClassRecord parentType [] []) todo would include primitivetypes later
