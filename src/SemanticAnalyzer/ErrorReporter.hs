{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ErrorReporter where

import Control.Monad.Extra (unlessM, whenJust)
import Control.Monad.Writer (tell)
import SemanticAnalyzer.IsType (IsType, (<==), lookupClass, toType)
import SemanticAnalyzer.SemanticAnalyzer (SemanticAnalyzerM)
import SemanticAnalyzer.TypeCheckError
       (MismatchSubtypeReporter, TypeCheckError, SubtypeReporter,
        UndefinedTypeReporter)
import SemanticAnalyzer.Maybe (killComputation, runNothing)

reportSubtypeError ::
     IsType a
  => IsType b =>
       MismatchSubtypeReporter -> a -> b -> SemanticAnalyzerM ()
reportSubtypeError reporter possibleSubtype ancestorType =
  unlessM (possibleSubtype <== ancestorType) $
  killComputation $ tell [reporter (toType possibleSubtype) (toType ancestorType)]

reportUndefinedType :: IsType a => UndefinedTypeReporter -> a -> SemanticAnalyzerM ()
reportUndefinedType reporter typeVal = reportUndefined (lookupClass typeVal) $ reporter (toType typeVal)

reportUndefined :: SemanticAnalyzerM a -> TypeCheckError -> SemanticAnalyzerM ()
reportUndefined semanticAnalyzer semanticError = runNothing semanticAnalyzer $ tell [semanticError]

checkSubtype ::
     IsType a
  => IsType b =>
       a -> b -> SubtypeReporter -> SemanticAnalyzerM ()
checkSubtype ancestorType parentType = checkSubtypeM ancestorType (Just parentType)

checkSubtypeM ::
     IsType a
  => IsType b =>
       a -> Maybe b -> SubtypeReporter -> SemanticAnalyzerM ()
checkSubtypeM declaredType maybeExpressionType (undefinedTypeReporter, mismatchSubtypeReporter) = do
  reportUndefinedType undefinedTypeReporter declaredType
  whenJust
    maybeExpressionType
    (\expressionType -> reportSubtypeError mismatchSubtypeReporter expressionType declaredType)
