{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.VariableIntroduction where

import Control.Monad.Trans.Maybe (MaybeT(..))

import qualified Parser.AST as AST
import SemanticAnalyzer.ErrorReporter (checkSubtypeM)
import SemanticAnalyzer.IsType (IsType,toType)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.TypeCheckError (IntroducedVariableReporter)
import SemanticAnalyzer.TypedAST (ExpressionT(..))
import SemanticAnalyzer.Type (Type)

data VariableEnvironmentInput =
  VariableEnvironmentInput String
                           String
                           (Maybe AST.Expression)
                           IntroducedVariableReporter

type VariableEnvironment a = Type -> Maybe ExpressionT -> SemanticAnalyzer a

checkIntroducedVariable ::
     IsType a
  => IsType b =>
       String -> a -> Maybe b -> IntroducedVariableReporter -> SemanticAnalyzerM ()
checkIntroducedVariable identifierName declaredType maybeExpressionT (undefinedTypeReporter, mismatchSubtypeReporter) =
  checkSubtypeM
    declaredType
    maybeExpressionT
    (undefinedTypeReporter identifierName, mismatchSubtypeReporter identifierName)

setupVariableIntroductionEnvironment ::
     VariableEnvironmentInput
  -> (AST.Expression -> SemanticAnalyzer ExpressionT)
  -> VariableEnvironment output
  -> SemanticAnalyzer output
setupVariableIntroductionEnvironment (VariableEnvironmentInput identifierName declaredType maybeExpression reporter) checker runEnvironment = do
  maybeExpressionT' <- runMaybeT maybeExpressionT
  _ <- runMaybeT $ checkIntroducedVariable identifierName declaredType maybeExpressionT' reporter
  runEnvironment (toType declaredType) maybeExpressionT'
  where
    maybeExpressionT = checker >>=? maybeExpression

(>>=?) :: Monad m => (a -> m b) -> Maybe a -> MaybeT m b
f >>=? maybeValue = MaybeT $ maybe (return Nothing) (fmap Just . f) maybeValue
