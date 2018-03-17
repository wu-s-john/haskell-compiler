{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticCheck where

import qualified Data.Map as M

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class (MethodMap, MethodRecord(..), getMethods)
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), LetBindingT(..), computeType)

import Control.Monad (unless,zipWithM)
import Control.Monad.State (get)
import Control.Monad.Writer (tell)
import SemanticAnalyzer.SemanticAnalyzer
import Control.Monad.Reader (ask)

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
semanticCheck (AST.LetExpr (AST.LetBinding newVariable newVariableType maybeInitialExpression evaluatingExpression)) --todo look for a way to refactor this more cleanly
 =
  (newVariable, newVariableType) /> do
    evaluatingExpressionT <- semanticCheck evaluatingExpression
    case maybeInitialExpression of
      Just initialExpression -> do
        initialExpressionT <- semanticCheck initialExpression
        let initialExpressionTypeName = computeType initialExpressionT
        isSubtype <- initialExpressionTypeName <== newVariableType
        unless isSubtype (tell [MismatchDeclarationType initialExpressionTypeName newVariableType])
        transformResult (Just initialExpressionT) evaluatingExpressionT
      Nothing -> transformResult Nothing evaluatingExpressionT
  where
    transformResult initialMaybeExpression evaluatingExpressionT =
      return $ LetExprT $ LetBindingT newVariable newVariableType initialMaybeExpression evaluatingExpressionT
semanticCheck AST.SelfVarExpr = return SelfVarExprT
semanticCheck (AST.MethodDispatch callerExpression calleeName calleeParameters) = do
  callerExpressionT <- semanticCheck callerExpression
  (_, classEnvironment) <- ask
  let callerExprClassName = computeType callerExpressionT
  case M.lookup "Foo" classEnvironment of
    Just classRecord -> checkCallee calleeName callerExpressionT (getMethods classRecord) calleeParameters
    Nothing -> tell [DispatchUndefinedClass callerExprClassName] >> errorMethodReturn callerExpressionT calleeName

errorMethodReturn :: ExpressionT -> T.Identifier -> SemanticAnalyzer ExpressionT
errorMethodReturn initialExpressionT calleeName = return (MethodDispatchT initialExpressionT calleeName [] "Object")

checkCallee :: T.Identifier -> ExpressionT -> MethodMap -> [AST.Expression] -> SemanticAnalyzer ExpressionT
checkCallee calleeName callerExpression classMethods parameters' =
  case M.lookup calleeName classMethods of
    Nothing -> tell [UndefinedMethod calleeName] >> errorMethodReturn callerExpression calleeName
    Just (MethodRecord _ arguments returnTypeName) -> do
      parametersT <- checkParameters calleeName arguments parameters'
      return (MethodDispatchT callerExpression calleeName parametersT returnTypeName)

checkParameters :: String -> [(T.Identifier, T.Type)] -> [AST.Expression] -> SemanticAnalyzer [ExpressionT]
checkParameters callMethodName formals expressions =
  if length formals /= length expressions
    then tell [WrongNumberParameters callMethodName] >> mapM semanticCheck expressions
    else zipWithM (uncurry checkParameter) formals expressions
  where
    checkParameter formalArgumentName formalTypeName actualParameterExpr = do
      actualParameterExprT <- semanticCheck actualParameterExpr
      let actualParameterTypeName = computeType actualParameterExprT
      isSubtype <- actualParameterTypeName <== formalTypeName
      unless isSubtype $
        tell [WrongParameterType callMethodName formalArgumentName formalTypeName actualParameterTypeName]
      return actualParameterExprT
