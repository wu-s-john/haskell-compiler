{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.MethodDispatch
  ( checkMethod
  ) where

import qualified Data.Map as M

import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class (MethodRecord(..), getMethods)

import Control.Monad (unless, zipWithM)
import Control.Monad.Extra (unlessM, void)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import Control.Monad.Writer (tell)
import Data.Maybe (isNothing)
import SemanticAnalyzer.ErrorReporter
       (reportSubtypeError, reportUndefined, reportUndefinedType)
import SemanticAnalyzer.IsType (IsType, lookupClass)
import SemanticAnalyzer.Maybe (continueComputation, runMaybe)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError (SemanticError(..))
import SemanticAnalyzer.Type (Type(TypeName))

checkMethod ::
     IsType a
  => IsType b =>
       String -> a -> [b] -> SemanticAnalyzer Type
checkMethod calleeName callerType calleeParameters = runMaybe (TypeName "Object") maybeResult
  where
    maybeClassRecord = lookupClass callerType
    maybeArguments = do
      (MethodRecord _ arguments _) <- maybeMethodRecord
      return arguments
    maybeMethodRecord = do
      callerTypeClassRecord <- maybeClassRecord
      let classMethods = getMethods callerTypeClassRecord
      MaybeT $ return (M.lookup calleeName classMethods)
    reportWrongNumberParameters = do
      arguments <- maybeArguments
      unless (length calleeParameters == length arguments) (tell [WrongNumberParameters calleeName])
    checkParameters = do
      arguments <- maybeArguments
      zipWithM (reportParameterSubtype calleeName) arguments calleeParameters
    reportUndefinedMethod = do
      reportUndefinedType DispatchUndefinedClass callerType
      reportUndefined maybeMethodRecord (UndefinedMethod calleeName)
    reportParameterErrors = do
      reportWrongNumberParameters
      void checkParameters
    maybeResult = do
      unlessM (lift $ isNothing <$> runMaybeT reportUndefinedMethod) reportParameterErrors
      (MethodRecord _ _ returnType') <- maybeMethodRecord
      return returnType'

reportParameterSubtype ::
     IsType a
  => IsType b =>
       String -> (T.Identifier, a) -> b -> SemanticAnalyzerM ()
reportParameterSubtype callMethodName (argumentName, argumentType) expressionType' =
  continueComputation $ reportSubtypeError (WrongParameterType callMethodName argumentName) argumentType expressionType'
