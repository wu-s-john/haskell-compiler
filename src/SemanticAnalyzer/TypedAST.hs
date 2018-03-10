{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiWayIf #-}

module SemanticAnalyzer.TypedAST where

import Control.Monad (unless)
import Control.Monad.Identity
import Control.Monad.Reader (Reader, ask)
import Control.Monad.State (State, get)
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
  deriving (Show, Eq)

data ExpressionT
  = IntegerExprT Int
  | PlusExprT { left :: ExpressionT
              , right :: ExpressionT }
  | StringExprT String
  | IdentifierExprT { name :: T.Identifier
                    , typeName :: T.Type }
  | LetExprT LetBindingT
  deriving (Show, Eq)

data LetBindingT
  = LetBindingT { getIdentifier :: T.Identifier
                , getType :: T.Type
                , getInitExpr :: Maybe ExpressionT
                , getExpr :: ExpressionT }
  | LetDeclaration { getIdentifier :: T.Identifier
                   , getType :: T.Type
                   , getInitExpr :: Maybe ExpressionT
                   , getLetBinding :: LetBindingT }
  deriving (Show, Eq)

computeType :: ExpressionT -> T.Type
computeType (IntegerExprT _) = "Int"
computeType (PlusExprT _ _) = "Int"
computeType (StringExprT _) = "String"
computeType (IdentifierExprT _ typeName') = typeName'

type ObjectEnvironment = M.Map T.Identifier T.Type

type SemanticAnalyzer = WriterT [SemanticError] (State ObjectEnvironment)

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

class (Monad m) =>
      Categorical a m where
  (<==) :: a -> a -> m Bool -- determines if the left left argument is a subset of the right
  (\/) :: a -> a -> m a

-- todo deal with primitive types
instance Categorical ClassRecord Identity where
  (ClassRecord possibleSubtype possibleParentRecord _ _) <== parentRecord@(ClassRecord parentType _ _ _)
    | possibleSubtype == parentType = return True
    | otherwise = possibleParentRecord <== parentRecord
  ObjectClass <== ClassRecord {} = return False
  _ <== ObjectClass = return True
  leftClassRecord \/ rightClassRecord = do
    let rightClassAncestors = computeAncestors rightClassRecord
    return $ lub leftClassRecord rightClassAncestors
    where computeAncestors ObjectClass = S.fromList ["Object"] -- invariant all classes should hold "Object" except for String and Int
          computeAncestors (ClassRecord className' parent' _ _) = className' `S.insert` computeAncestors parent'
          lub classRecord@(ClassRecord className' parent' _ _) ancestors
            | className' `elem` ancestors = classRecord
            | otherwise = lub parent' ancestors
          lub ObjectClass _ = ObjectClass

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


getClassRecord :: T.Type -> Reader ClassEnvironment ClassRecord
getClassRecord typeName' = do
  classEnvironment <- ask
  if | typeName' == "Object" -> return ObjectClass
     | otherwise -> case typeName' `M.lookup` classEnvironment of
                        Just classRecord -> return classRecord
                        Nothing -> return (ClassRecord typeName' ObjectClass M.empty M.empty)
     -- | parentType `elem` primitiveTypes -> (ClassRecord parentType [] []) todo would include primitivetypes later
