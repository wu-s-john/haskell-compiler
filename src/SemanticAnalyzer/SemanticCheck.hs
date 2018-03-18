{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticCheck
  ( semanticCheck
  ) where

import qualified Data.Map as M

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (MethodMap, MethodRecord(..), getMethods)
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), LetBindingT(..), computeType)

import Control.Monad (unless, zipWithM)
import Control.Monad.Reader (ask)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import Data.String (fromString)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.Type (Type(TypeName))

semanticCheck :: AST.Expression -> SemanticAnalyzer ExpressionT
semanticCheck (AST.IntegerExpr value) = return (IntegerExprT value)
semanticCheck (AST.StringExpr value) = return (StringExprT value)
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
    Nothing -> tell [UndeclaredIdentifier identifierName] >> return (IdentifierExprT identifierName $ TypeName "Object")
    Just identifierType -> return (IdentifierExprT identifierName identifierType)
semanticCheck (AST.LetExpr (AST.LetBinding newVariable newVariableTypeName maybeInitialExpression evaluatingExpression)) --todo look for a way to refactor this more cleanly
 =
  (newVariable, newVariableType) /> do
    evaluatingExpressionT <- semanticCheck evaluatingExpression
    case maybeInitialExpression of
      Just initialExpression -> do
        initialExpressionT <- semanticCheck initialExpression
        let initialExpressionType = computeType initialExpressionT
        isSubtype <- initialExpressionType <== newVariableType
        unless isSubtype (tell [MismatchDeclarationType initialExpressionType newVariableType])
        transformResult (Just initialExpressionT) evaluatingExpressionT
      Nothing -> transformResult Nothing evaluatingExpressionT
  where
    newVariableType = fromString newVariableTypeName
    transformResult initialMaybeExpression evaluatingExpressionT =
      return $ LetExprT $ LetBindingT newVariable newVariableType initialMaybeExpression evaluatingExpressionT
semanticCheck AST.SelfVarExpr = return SelfVarExprT
semanticCheck (AST.MethodDispatch callerExpression calleeName calleeParameters) = do
  callerExpressionT <- semanticCheck callerExpression
  (_, classEnvironment) <- ask
  case M.lookup "Foo" classEnvironment of
    Just classRecord -> checkCallee calleeName callerExpressionT (getMethods classRecord) calleeParameters
    Nothing ->
      tell [DispatchUndefinedClass (computeType callerExpressionT)] >> errorMethodReturn callerExpressionT calleeName

errorMethodReturn :: ExpressionT -> T.Identifier -> SemanticAnalyzer ExpressionT
errorMethodReturn initialExpressionT calleeName =
  return (MethodDispatchT initialExpressionT calleeName [] (TypeName "Object"))

checkCallee :: T.Identifier -> ExpressionT -> MethodMap -> [AST.Expression] -> SemanticAnalyzer ExpressionT
checkCallee calleeName callerExpression classMethods methodArguments =
  case M.lookup calleeName classMethods of
    Nothing -> tell [UndefinedMethod calleeName] >> errorMethodReturn callerExpression calleeName
    Just (MethodRecord _ arguments returnTypeName) -> do
      parametersT <- checkParameters calleeName arguments methodArguments
      return (MethodDispatchT callerExpression calleeName parametersT returnTypeName)

checkParameters :: String -> [(T.Identifier, Type)] -> [AST.Expression] -> SemanticAnalyzer [ExpressionT]
checkParameters callMethodName formals expressions =
  if length formals /= length expressions
    then tell [WrongNumberParameters callMethodName] >> mapM semanticCheck expressions
    else zipWithM (uncurry checkParameter) formals expressions
  where
    checkParameter formalArgumentName formalArgumentType parameterExpr = do
      actualParameterExprT <- semanticCheck parameterExpr
      let actualParameterTypeName = computeType actualParameterExprT
      isSubtype <- actualParameterTypeName <== formalArgumentType
      unless isSubtype $
        tell [WrongParameterType callMethodName formalArgumentName formalArgumentType actualParameterTypeName]
      return actualParameterExprT
