{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.SemanticCheck
  ( semanticCheck
  , TypeInferrable
  ) where

import qualified Data.Map as M

import qualified Parser.AST as AST
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), FeatureT(..), FormalT(..), LetBindingT(..),
        computeType)
import SemanticAnalyzer.VariableIntroduction

import Control.Monad (unless)
import Control.Monad.Extra ()
import Control.Monad.State (get)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer (tell)
import Data.String (fromString)
import SemanticAnalyzer.ErrorReporter
       (checkSubtype, reportSubtypeError, reportUndefinedType, runMaybe)
import SemanticAnalyzer.MethodDispatch (checkMethod)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError (SemanticError(..))
import SemanticAnalyzer.Type (Type(SELF_TYPE, TypeName))

class TypeInferrable a b | b -> a where
  semanticCheck :: a -> SemanticAnalyzer b

instance TypeInferrable AST.Feature FeatureT where
  semanticCheck (AST.Method methodString formals returnTypeName expression) = do
    _ <- runMaybeT reportUndefinedParameterTypes
    _ <- runMaybeT reportUndefinedReturnType
    introduceParameters formalsT $ do
      expressionT <- semanticCheck expression
      _ <-
        runMaybeT $
        reportSubtypeError (WrongSubtypeMethod methodString) (lookupClass expressionT) maybeReturnTypeClassRecord
      return $ MethodT methodString formalsT (TypeName returnTypeName) expressionT
    where
      reportUndefinedParameterTypes = mapM_ reportUndefinedParameterType formals
      reportUndefinedParameterType (AST.Formal identifier typeString) =
        reportUndefinedType (UndefinedParameterType identifier) typeString
      toFormalT (AST.Formal identifierName typeString) = FormalT identifierName (fromString typeString)
      formalsT = map toFormalT formals
      reportUndefinedReturnType = reportUndefinedType (UndefinedReturnType methodString) returnTypeName
      maybeReturnTypeClassRecord = lookupClass returnTypeName
  semanticCheck (AST.Attribute identifierName declaredTypeName maybeExpression) =
    setupVariableIntroductionEnvironment
      (VariableEnvironmentInput identifierName declaredTypeName maybeExpression reporter)
      semanticCheck
      createAttributeT
    where
      createAttributeT declaredTypeVal maybeExpressionT =
        return $ AttributeT identifierName declaredTypeVal maybeExpressionT
      reporter = (AttributeUndefinedDeclareType, WrongSubtypeAttribute)

introduceParameters :: [FormalT] -> SemanticAnalyzer a -> SemanticAnalyzer a
introduceParameters [] semanticAnalyzer = semanticAnalyzer
introduceParameters (FormalT identifierName type':formalTail) semanticAnalyzer =
  (identifierName, type') /> introduceParameters formalTail semanticAnalyzer

instance TypeInferrable AST.Expression ExpressionT where
  semanticCheck (AST.IntegerExpr value) = return (IntegerExprT value)
  semanticCheck (AST.StringExpr value) = return (StringExprT value)
  semanticCheck (AST.NewExpr typeString) =
    case fromString typeString of
      SELF_TYPE -> return $ NewExprT typeString SELF_TYPE
      (TypeName _) ->
        runMaybe
          (tell [UndefinedNewType typeString] >> return (NewExprT typeString (TypeName "Object")))
          (\_ -> return $ NewExprT typeString (fromString typeString))
          (lookupClass typeString)
  semanticCheck (AST.PlusExpr leftExpression rightExpression) = do
    annotatedLeft <- semanticCheck leftExpression
    annotatedRight <- semanticCheck rightExpression
    let leftType = computeType annotatedLeft
    let rightType = computeType annotatedRight
    unless
      (computeType annotatedLeft == TypeName "Int" && computeType annotatedRight == TypeName "Int")
      (tell [NonIntArgumentsPlus leftType rightType])
    return $ PlusExprT annotatedLeft annotatedRight
  semanticCheck (AST.IdentifierExpr identifierName) = do
    objectEnvironment <- get
    case identifierName `M.lookup` objectEnvironment of
      Nothing ->
        tell [UndeclaredIdentifier identifierName] >> return (IdentifierExprT identifierName $ TypeName "Object")
      Just identifierType -> return (IdentifierExprT identifierName identifierType)
  semanticCheck (AST.LetExpr (AST.LetBinding newVariable declaredTypeName maybeExpression evaluatingExpression)) =
    setupVariableIntroductionEnvironment input semanticCheck invokeLetExprT
    where
      input = VariableEnvironmentInput newVariable declaredTypeName maybeExpression reporter
      reporter = (LetUndefinedDeclareType, WrongSubtypeLet)
      invokeLetExprT declaredTypeVal maybeExpressionT =
        (newVariable, declaredTypeVal) /> do
          evaluatingExpressionT <- semanticCheck evaluatingExpression
          return $ LetExprT $ LetBindingT newVariable declaredTypeVal maybeExpressionT evaluatingExpressionT
  semanticCheck AST.SelfVarExpr = return SelfVarExprT
  semanticCheck (AST.MethodDispatch callerExpression calleeName calleeParameters) = do
    callerExpressionT <- semanticCheck callerExpression
    calleeParametersT <- mapM semanticCheck calleeParameters
    let callerExpressionType = computeType callerExpressionT
    inferredReturnType <- checkMethod calleeName callerExpressionType calleeParametersT
    return (MethodDispatchT callerExpressionT calleeName calleeParametersT inferredReturnType)
  semanticCheck (AST.StaticMethodDispatch callerExpression staticTypeString calleeName calleeParameters) = do
    callerExpressionT <- semanticCheck callerExpression
    calleeParametersT <- mapM semanticCheck calleeParameters
    subtypeReport <-
      runMaybeT $ checkSubtype staticTypeString callerExpressionT (UndefinedStaticDispatch, WrongSubtypeStaticDispatch)
    inferredReturnType <-
      case subtypeReport of
        Nothing -> return (TypeName "Object")
        Just _ -> checkMethod calleeName (fromString staticTypeString) calleeParametersT
    return $ StaticMethodDispatchT callerExpressionT staticTypeString calleeName calleeParametersT inferredReturnType
