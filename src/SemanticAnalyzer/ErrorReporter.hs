{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ErrorReporter where

import Control.Monad (void)
import Control.Monad.Extra (maybeM, unlessM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.Maybe (MaybeT(MaybeT), runMaybeT)
import Control.Monad.Writer (tell)
import Data.Maybe (isJust)
import Data.String (fromString)
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Class (ClassRecord(ClassRecord), MethodRecord)
import SemanticAnalyzer.SemanticAnalyzer (SemanticAnalyzerM)
import SemanticAnalyzer.SemanticCheckUtil ((<==?), lookupClass)
import SemanticAnalyzer.SemanticError
       (MismatchSubtypeReporter, SemanticError(UndefinedMethod),
        SubtypeReporter, UndefinedTypeReporter)
import SemanticAnalyzer.Type (Type)
import SemanticAnalyzer.TypedAST (ExpressionT)

class ReportableSubtypeError a where
  reportSubtypeError :: MismatchSubtypeReporter -> SemanticAnalyzerM a -> SemanticAnalyzerM a -> SemanticAnalyzerM ()

instance ReportableSubtypeError ClassRecord where
  reportSubtypeError reporter maybePossibleSubtypeClassRecord maybeAncestorTypeClassRecord = do
    (ClassRecord possibleSubtypeName _ _ _) <- maybePossibleSubtypeClassRecord
    (ClassRecord ancestorName _ _ _) <- maybeAncestorTypeClassRecord
    unlessM (maybePossibleSubtypeClassRecord <==? maybeAncestorTypeClassRecord) $
      killComputation $ tell [reporter (fromString possibleSubtypeName) (fromString ancestorName)]

instance ReportableSubtypeError Type where
  reportSubtypeError reporter maybePossibleSubtype maybeAncestorType = do
    possibleSubtype <- maybePossibleSubtype
    ancestorType <- maybeAncestorType
    reportSubtypeError reporter (lookupClass possibleSubtype) (lookupClass ancestorType)

reportUndefinedType :: UndefinedTypeReporter -> Identifier -> SemanticAnalyzerM ()
reportUndefinedType reporter typeString = runNothing lookupComputation $ tell [reporter (fromString typeString)]
  where
    lookupComputation = lookupClass typeString

reportUndefinedRecord :: String -> SemanticAnalyzerM MethodRecord -> SemanticAnalyzerM ()
reportUndefinedRecord calleeName maybeMethodRecord = runNothing maybeMethodRecord $ tell [UndefinedMethod calleeName]

checkSubtype :: String -> ExpressionT -> SubtypeReporter -> SemanticAnalyzerM ()
checkSubtype declaredTypeName expressionT = checkSubtypeM declaredTypeName (Just expressionT)

checkSubtypeM :: String -> Maybe ExpressionT -> SubtypeReporter -> SemanticAnalyzerM ()
checkSubtypeM declaredTypeName maybeExpressionT (undefinedTypeReporter, mismatchSubtypeReporter) = do
  reportUndefinedType undefinedTypeReporter declaredTypeName
  reportSubtypeError mismatchSubtypeReporter maybeExpressionTypeClassRecord maybeDeclaredTypeClassRecord
  where
    maybeDeclaredTypeClassRecord = lookupClass declaredTypeName
    maybeExpressionTypeClassRecord = maybe (MaybeT $ return Nothing) lookupClass maybeExpressionT

runNothing :: Monad m => MaybeT m a -> m () -> MaybeT m ()
runNothing maybeMonad action = do
  lift $ unlessM (isJust <$> runMaybeT maybeMonad) action
  void maybeMonad

runMaybe :: Monad m => m b -> (a -> m b) -> MaybeT m a -> m b
runMaybe defaultMonad f maybeT = maybeM defaultMonad f (runMaybeT maybeT)

killComputation :: Monad m => MaybeT m () -> MaybeT m ()
killComputation computation = computation >> (MaybeT $ return Nothing)

continueComputation :: Monad m => MaybeT m () -> MaybeT m ()
continueComputation computation =
  MaybeT $ do
    unlessM (isJust <$> runMaybeT computation) (return ())
    return (Just ())
