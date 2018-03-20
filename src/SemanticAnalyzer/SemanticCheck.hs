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
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (ClassRecord(..), MethodRecord(..), getMethods)
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), FeatureT(..), FormalT(..), LetBindingT(..),
        computeType)

import Control.Monad (unless, zipWithM)
import Control.Monad.Extra (andM, ifM, maybeM, unlessM)
import Control.Monad.State (get)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell)
import Data.Maybe (isNothing)
import Data.String (fromString)
import SemanticAnalyzer.ErrorReporter
       (reportSubtypeError, reportUndefinedType)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError (SemanticError(..))
import SemanticAnalyzer.Type (Type(SELF_TYPE, TypeName))

class TypeInferrable a b | b -> a where
  semanticCheck :: a -> SemanticAnalyzer b

instance TypeInferrable AST.Feature FeatureT where
  semanticCheck (AST.Method methodString formals returnTypeName expression) = do
    reportUndefinedParameterTypes
    reportUndefinedReturnType
    introduceParameters formalsT $ do
      expressionT <- semanticCheck expression
      reportSubtypeError WrongSubtypeMethod methodString (lookupClass expressionT) maybeReturnTypeClassRecord
      return $ MethodT methodString formalsT (TypeName returnTypeName) expressionT
    where
      reportUndefinedParameterTypes = mapM_ reportUndefinedParameterType formals
      reportUndefinedParameterType (AST.Formal identifier typeString) =
        reportUndefinedType UndefinedParameterType identifier typeString
      toFormalT (AST.Formal identifierName typeString) = FormalT identifierName (fromString typeString)
      formalsT = map toFormalT formals
      reportUndefinedReturnType = reportUndefinedType UndefinedReturnType methodString returnTypeName
      maybeReturnTypeClassRecord = lookupClass returnTypeName
  semanticCheck (AST.Attribute identifierName declaredTypeName maybeExpression) = do
    maybeExpressionT' <- runMaybeT maybeExpressionT
    _ <-
      reportSubtypeError
        WrongSubtypeAttribute
        identifierName
        maybeExpressionTypeClassRecord
        maybeDeclaredTypeClassRecord
    undefinedDeclareTypeReport
    return $ AttributeT identifierName declaredTypeVal maybeExpressionT'
    where
      maybeExpressionT :: SemanticAnalyzerM ExpressionT
      maybeExpressionT = MaybeT $ maybe (return Nothing) (fmap Just . semanticCheck) maybeExpression
      maybeDeclaredTypeClassRecord = lookupClass declaredTypeName
      undefinedDeclareTypeReport = reportUndefinedType AttributeUndefinedDeclareType identifierName declaredTypeName
      maybeExpressionTypeClassRecord :: SemanticAnalyzerM ClassRecord
      maybeExpressionTypeClassRecord = lookupClass maybeExpressionT
      declaredTypeVal = fromString declaredTypeName

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
        maybeM
          (tell [UndefinedNewType typeString] >> return (NewExprT typeString (TypeName "Object")))
          (\_ -> return $ NewExprT typeString (fromString typeString))
          (lookupClass'' typeString)
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
  semanticCheck (AST.LetExpr (AST.LetBinding newVariable newVariableTypeName maybeInitialExpression evaluatingExpression)) --todo look for a way to refactor this more cleanly
   =
    (newVariable, newVariableType) /> do
      evaluatingExpressionT <- semanticCheck evaluatingExpression
      maybeM
        (transformResult Nothing evaluatingExpressionT)
        (\initialExpression -- todo initial expression should not consider the injected variable
          -> do
           initialExpressionT <- semanticCheck initialExpression
           let initialExpressionType = computeType initialExpressionT
           unlessM
             (initialExpressionType <== newVariableType)
             (tell [WrongSubtypeLet newVariable initialExpressionType newVariableType])
                      -- todo deal with declared type is undefined
           transformResult (Just initialExpressionT) evaluatingExpressionT)
        (return maybeInitialExpression)
    where
      newVariableType = fromString newVariableTypeName
      transformResult initialMaybeExpression evaluatingExpressionT =
        return $ LetExprT $ LetBindingT newVariable newVariableType initialMaybeExpression evaluatingExpressionT
  semanticCheck AST.SelfVarExpr = return SelfVarExprT
  semanticCheck (AST.MethodDispatch callerExpression calleeName calleeParameters) = do
    callerExpressionT <- semanticCheck callerExpression
    (TypeName expressionTypeName) <- coerceType $ computeType callerExpressionT
    checkMethod callerExpressionT expressionTypeName calleeName calleeParameters
  semanticCheck (AST.StaticMethodDispatch callerExpression staticType calleeName calleeParameters) = do
    callerExpressionT <- semanticCheck callerExpression
    parametersT <- mapM semanticCheck calleeParameters
    let staticError = StaticMethodDispatchT callerExpressionT staticType calleeName parametersT (TypeName "Object")
    isContinuable <-
      andM
        [ checkAndReportError (isNothing <$> lookupClass'' staticType) (tell [UndefinedStaticDispatch staticType])
        , checkAndReportError
            (not <$> (computeType callerExpressionT <== fromString staticType))
            (tell [WrongStaticDispatch (computeType callerExpressionT) (fromString staticType)])
        ]
    if isContinuable
      then do
        (MethodDispatchT _ _ _ typeResult) <- checkMethod callerExpressionT staticType calleeName calleeParameters
        return $ StaticMethodDispatchT callerExpressionT staticType calleeName parametersT typeResult
      else return staticError
    where
      checkAndReportError isError report = ifM isError (report >> return False) (return True)

checkMethod :: ExpressionT -> String -> T.Identifier -> [AST.Expression] -> SemanticAnalyzer ExpressionT
checkMethod callerExpressionT callerExpressionTypeName calleeName calleeParameters =
  maybeM
    (tell [DispatchUndefinedClass (computeType callerExpressionT)] >> errorMethodReturn callerExpressionT calleeName)
    (\classRecord -> checkCallee calleeName callerExpressionT classRecord calleeParameters)
    (lookupClass'' callerExpressionTypeName)

errorMethodReturn :: ExpressionT -> T.Identifier -> SemanticAnalyzer ExpressionT
errorMethodReturn initialExpressionT calleeName =
  return (MethodDispatchT initialExpressionT calleeName [] (TypeName "Object"))

checkCallee :: T.Identifier -> ExpressionT -> ClassRecord -> [AST.Expression] -> SemanticAnalyzer ExpressionT
checkCallee calleeName callerExpression classRecord methodArguments =
  let classMethods = getMethods classRecord
  in case M.lookup calleeName classMethods of
       Nothing -> tell [UndefinedMethod calleeName] >> errorMethodReturn callerExpression calleeName
       Just (MethodRecord _ arguments returnTypeName) -> do
         parametersT <- checkParameters calleeName arguments methodArguments
         returnTypeName' <- coerceType returnTypeName
         return (MethodDispatchT callerExpression calleeName parametersT returnTypeName')

checkParameters :: String -> [(T.Identifier, Type)] -> [AST.Expression] -> SemanticAnalyzer [ExpressionT]
checkParameters callMethodName formals expressions =
  if length formals /= length expressions
    then tell [WrongNumberParameters callMethodName] >> mapM semanticCheck expressions
    else zipWithM (uncurry checkParameter) formals expressions
  where
    checkParameter formalArgumentName formalArgumentType parameterExpr = do
      actualParameterExprT <- semanticCheck parameterExpr
      let actualParameterTypeName = computeType actualParameterExprT
      unlessM (actualParameterTypeName <== formalArgumentType) $
        tell [WrongParameterType callMethodName formalArgumentName formalArgumentType actualParameterTypeName]
      return actualParameterExprT
