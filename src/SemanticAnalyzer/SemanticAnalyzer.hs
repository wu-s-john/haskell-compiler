{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzer where

import Control.Monad.Reader (ReaderT)
import Control.Monad.State (State)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as M
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.Type (Type)
import SemanticAnalyzer.SemanticError
import Control.Monad.Trans.Maybe (MaybeT)

type ObjectEnvironment = M.Map Identifier Type
type SemanticAnalyzer = ReaderT (String, ClassEnvironment) (WriterT [SemanticError] (State ObjectEnvironment))
type SemanticAnalyzerM a = MaybeT SemanticAnalyzer a

