{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.MethodDispatch
  ( checkMethod
  ) where

import qualified Data.Map as M

import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class (MethodRecord(..), getMethods)
import SemanticAnalyzer.SemanticCheckUtil
import SemanticAnalyzer.TypedAST (ExpressionT, computeType)

import Control.Monad (unless, zipWithM)
import Control.Monad.Extra (unlessM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer (tell)
import Data.Maybe (isNothing)
import SemanticAnalyzer.ErrorReporter
       (continueComputation, reportSubtypeError, reportUndefinedRecord,
        reportUndefinedType, runMaybe, runNothing)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError (SemanticError(..))
import SemanticAnalyzer.Type (Type(TypeName))

checkMethod :: String -> Type -> [ExpressionT] -> SemanticAnalyzer Type
checkMethod calleeName callerType calleeParameters = runMaybe (return $ TypeName "Object") return maybeResult
  where
    maybeCallerTypeClassRecord = lookupClass callerType
    maybeArguments = do
      (MethodRecord _ arguments _) <- maybeMethodRecord
      return arguments
    maybeMethodRecord = do
      callerTypeClassRecord <- maybeCallerTypeClassRecord
      let classMethods = getMethods callerTypeClassRecord
      MaybeT $ return (M.lookup calleeName classMethods)
    maybeReturnType = do
      (MethodRecord _ _ returnType') <- maybeMethodRecord
      return returnType'
    reportWrongNumberParameters = do
      arguments <- maybeArguments
      unless (length calleeParameters == length arguments) (tell [WrongNumberParameters calleeName])
    checkParameters = do
      arguments <- maybeArguments
      zipWithM (reportParameterSubtype calleeName) arguments calleeParameters
    reportUndefinedMethod = do
      typeString <- lift $ toString callerType
      reportUndefinedType DispatchUndefinedClass typeString
      reportUndefinedRecord calleeName maybeMethodRecord
    reportParameterErrors = do
      reportWrongNumberParameters
      void checkParameters
    maybeResult = do
      unlessM (lift $ isNothing <$> runMaybeT reportUndefinedMethod) reportParameterErrors
      maybeReturnType

reportParameterSubtype :: String -> (T.Identifier, Type) -> ExpressionT -> SemanticAnalyzerM ()
reportParameterSubtype callMethodName (argumentName, argumentType) parameterExprT = do
  let actualParameterTypeName = computeType parameterExprT
  continueComputation $
    reportSubtypeError
      (WrongParameterType callMethodName argumentName)
      (lookupClass argumentType)
      (lookupClass actualParameterTypeName)
