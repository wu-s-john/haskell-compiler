{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ErrorReporter where

import Control.Monad (void)
import Control.Monad.Extra (unlessM)
import Control.Monad.Trans.Maybe (runMaybeT)
import Control.Monad.Writer (tell)
import Data.Maybe (isJust)
import Data.String (fromString)
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Class (ClassRecord(ClassRecord))
import SemanticAnalyzer.SemanticAnalyzer
       (SemanticAnalyzer, SemanticAnalyzerM, lookupClass)
import SemanticAnalyzer.SemanticCheckUtil ((<==?))
import SemanticAnalyzer.SemanticError (MismatchSubtypeReporter, UndefinedTypeReporter)
import SemanticAnalyzer.Type (Type(TypeName))

reportSubtypeError ::
     MismatchSubtypeReporter
  -> Identifier
  -> SemanticAnalyzerM ClassRecord
  -> SemanticAnalyzerM ClassRecord
  -> SemanticAnalyzer ()
reportSubtypeError reporter name' maybePossibleSubtypeClassRecord maybeAncestorTypeClassRecord =
  void $
  runMaybeT $ do
    (ClassRecord possibleSubtypeName _ _ _) <- maybePossibleSubtypeClassRecord
    (ClassRecord ancestorName _ _ _) <- maybeAncestorTypeClassRecord
    unlessM (maybePossibleSubtypeClassRecord <==? maybeAncestorTypeClassRecord) $
      tell [reporter name' (fromString possibleSubtypeName) (fromString ancestorName)]

reportUndefinedType :: UndefinedTypeReporter -> Identifier -> String -> SemanticAnalyzer ()
reportUndefinedType reporter identifier typeString =
  unlessM (isJust <$> lookupClass typeString) $ tell [reporter identifier (TypeName typeString)]
