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

import Control.Monad (unless, void, zipWithM)
import Control.Monad.Extra (andM, ifM, maybeM, unlessM)
import Control.Monad.Identity (runIdentity)
import Control.Monad.State (get)
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Writer (tell)
import Data.Maybe (isJust, isNothing)
import Data.String (fromString)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.Type (Type(SELF_TYPE, TypeName))

class TypeInferrable a b | b -> a where
  semanticCheck :: a -> SemanticAnalyzer b

type UndefinedTypeReporter = T.Identifier -> Type -> SemanticError

type SemanticAnalyzerM a = MaybeT SemanticAnalyzer a

instance TypeInferrable AST.Feature FeatureT where
  semanticCheck (AST.Method methodString formals returnTypeName expression) = do
    expressionT <- semanticCheck expression
    reportUndefinedParameterTypes
    reportUndefinedReturnType
    reportSubtypeError WrongSubtypeMethod methodString (lookupClass' expressionT) maybeReturnTypeClassRecord
    return $ MethodT methodString formalsT (TypeName returnTypeName) expressionT
    where
      reportUndefinedParameterTypes = mapM_ reportUndefinedParameterType formals
      reportUndefinedParameterType (AST.Formal identifier typeString) =
        reportUndefinedType UndefinedParameterType identifier typeString
      toFormalT (AST.Formal identifierName typeString) = FormalT identifierName (fromString typeString)
      formalsT = map toFormalT formals
      reportUndefinedReturnType = reportUndefinedType UndefinedReturnType methodString returnTypeName
      maybeReturnTypeClassRecord = lookupClass' returnTypeName
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
      maybeExpressionT =
        MaybeT $ maybe (return Nothing) (fmap Just . semanticCheck) maybeExpression
      maybeDeclaredTypeClassRecord = lookupClass' declaredTypeName
      undefinedDeclareTypeReport = reportUndefinedType AttributeUndefinedDeclareType identifierName declaredTypeName
      maybeExpressionTypeClassRecord :: SemanticAnalyzerM ClassRecord
      maybeExpressionTypeClassRecord = lookupClass' maybeExpressionT
      declaredTypeVal = fromString declaredTypeName

(<==?) :: SemanticAnalyzerM ClassRecord -> SemanticAnalyzerM ClassRecord -> SemanticAnalyzerM Bool
maybePossibleSubtypeClassRecord <==? maybeAncestorTypeClassRecord = do
  possibleSubtypeClassRecord <- maybePossibleSubtypeClassRecord
  ancestorTypeClassRecord <- maybeAncestorTypeClassRecord
  return $ runIdentity (ancestorTypeClassRecord <== possibleSubtypeClassRecord)

reportSubtypeError ::
     (T.Identifier -> Type -> Type -> SemanticError)
  -> T.Identifier
  -> SemanticAnalyzerM ClassRecord
  -> SemanticAnalyzerM ClassRecord
  -> SemanticAnalyzer ()
reportSubtypeError reporter name' maybePossibleSubtypeClassRecord maybeAncestorTypeClassRecord =
  void $
  runMaybeT $ do
    (ClassRecord possibleSubtypeName _ _ _) <- maybePossibleSubtypeClassRecord
    (ClassRecord ancestorName _ _ _) <- maybeAncestorTypeClassRecord
    unlessM (maybePossibleSubtypeClassRecord <==? maybeAncestorTypeClassRecord) $
      tell [reporter name' (TypeName possibleSubtypeName) (TypeName ancestorName)]

reportUndefinedType :: UndefinedTypeReporter -> T.Identifier -> String -> SemanticAnalyzer ()
reportUndefinedType reporter identifier typeString =
  unlessM (isJust <$> lookupClass typeString) $ tell [reporter identifier (TypeName typeString)]

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
             (tell [MismatchDeclarationType initialExpressionType newVariableType])
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
        [ checkAndReportError (isNothing <$> lookupClass staticType) (tell [UndefinedStaticDispatch staticType])
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
    (lookupClass callerExpressionTypeName)

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
