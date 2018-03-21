{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.VariableIntroduction where

import Control.Monad.Trans.Maybe (MaybeT(..))

import Data.String (fromString)
import qualified Parser.AST as AST
import SemanticAnalyzer.ErrorReporter (checkSubtypeM)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError (IntroducedVariableReporter)
import SemanticAnalyzer.Type (Type)
import SemanticAnalyzer.TypedAST (ExpressionT(..))

data VariableEnvironmentInput =
  VariableEnvironmentInput String
                           String
                           (Maybe AST.Expression)
                           IntroducedVariableReporter

type VariableEnvironment a = Type -> Maybe ExpressionT -> SemanticAnalyzer a

checkIntroducedVariable :: String -> String -> Maybe ExpressionT -> IntroducedVariableReporter -> SemanticAnalyzerM ()
checkIntroducedVariable identifierName declaredTypeName maybeExpressionT (undefinedTypeReporter, mismatchSubtypeReporter) =
  checkSubtypeM
    declaredTypeName
    maybeExpressionT
    (undefinedTypeReporter identifierName, mismatchSubtypeReporter identifierName)

setupVariableIntroductionEnvironment ::
     VariableEnvironmentInput
  -> (AST.Expression -> SemanticAnalyzer ExpressionT)
  -> VariableEnvironment a
  -> SemanticAnalyzer a
setupVariableIntroductionEnvironment (VariableEnvironmentInput identifierName declaredTypeName maybeExpression reporter) checker setupEnvironment = do
  maybeExpressionT' <- runMaybeT maybeExpressionT
  _ <- runMaybeT $ checkIntroducedVariable identifierName declaredTypeName maybeExpressionT' reporter
  setupEnvironment declaredTypeVal maybeExpressionT'
  where
    maybeExpressionT = checker >>=? maybeExpression
    declaredTypeVal = fromString declaredTypeName

(>>=?) :: Monad m => (a -> m b) -> Maybe a -> MaybeT m b
f >>=? maybeValue = MaybeT $ maybe (return Nothing) (fmap Just . f) maybeValue
