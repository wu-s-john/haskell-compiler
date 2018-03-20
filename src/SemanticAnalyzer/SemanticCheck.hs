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
       (ClassRecord, MethodRecord(..), getMethods)
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), FeatureT(..), LetBindingT(..), computeType)

import Control.Monad (unless, zipWithM)
import Control.Monad.Extra (andM, ifM, maybeM, unlessM, (>=>), (&&^))
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Control.Monad.Trans.Maybe (MaybeT(..), mapMaybeT)
import Data.Maybe (isNothing,isJust)
import Data.String (fromString)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.Type (Type(SELF_TYPE, TypeName))
import Control.Monad.Identity (runIdentity)

class TypeInferrable a b | b -> a where
  semanticCheck :: a -> SemanticAnalyzer b

data Cond a b =
  (b -> SemanticAnalyzer a) :< SemanticAnalyzer a

infixl 0 ?->
infixl 1 :<

-- checks if an expression exists
(?->) :: Maybe b -> Cond (Maybe a) b -> MaybeT SemanticAnalyzer a
maybeValue ?-> justExpression :< nothingExpression = MaybeT $ maybeM nothingExpression justExpression (return maybeValue)

instance TypeInferrable AST.Feature FeatureT where
  semanticCheck (AST.Attribute identifierName declaredTypeName maybeExpression) = do
    maybeExpressionT' <- (runMaybeT maybeExpressionT)
    _ <- runMaybeT reportSubtypeError
    undefinedDeclareTypeReport
    return $ AttributeT identifierName declaredTypeVal maybeExpressionT'
    where maybeExpressionT :: MaybeT SemanticAnalyzer ExpressionT
          maybeExpressionT = maybeExpression ?-> (\expression -> Just <$> semanticCheck expression ) :< return Nothing
          maybeDeclaredTypeClassRecord = MaybeT $ lookupClass declaredTypeName
          undefinedDeclareTypeReport = do
            unlessM (isJust <$> runMaybeT maybeDeclaredTypeClassRecord) $
              tell [AttributeUndefinedDeclareType identifierName declaredTypeVal]
          maybeExpressionType :: MaybeT SemanticAnalyzer Type
          maybeExpressionType = computeType <$> maybeExpressionT
          maybeExpressionTypeClassRecord :: MaybeT SemanticAnalyzer ClassRecord
          maybeExpressionTypeClassRecord = mapMaybeT (maybeM (return Nothing) (toString >=> lookupClass)) maybeExpressionType
          isSubtype = do
            declaredTypeClassRecord <- maybeDeclaredTypeClassRecord
            expressionTypeClassRecord <- maybeExpressionTypeClassRecord
            return $ runIdentity (expressionTypeClassRecord <== declaredTypeClassRecord)
          reportSubtypeError = do
            expressionTypeVal <- maybeExpressionType
            unlessM isSubtype $ tell [WrongSubtypeAttribute identifierName expressionTypeVal declaredTypeVal]
          declaredTypeVal = fromString declaredTypeName


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
        (\initialExpression -> do -- todo initial expression should not consider the injected variable
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
            (tell [WrongStaticDispatch (computeType callerExpressionT) (fromString  staticType)])
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


coerceType :: Type -> SemanticAnalyzer Type
coerceType SELF_TYPE = invokeClassName $ \typeName' -> return $ TypeName typeName'
coerceType type'@(TypeName _) = return type'

toString :: Type -> SemanticAnalyzer String
toString typeVal = do
  (TypeName typeString) <- coerceType typeVal
  return typeString
