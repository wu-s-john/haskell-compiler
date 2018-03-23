{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzer where

import Control.Monad.RWS.Lazy (RWS)
import Control.Monad.Reader (ReaderT)
import Control.Monad.Trans.Maybe (MaybeT)
import Control.Monad.Writer.Lazy (Writer)
import qualified Data.Map as M
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.TypeCheckError
import SemanticAnalyzer.Type (Type)

type ObjectEnvironment = M.Map Identifier Type

type SemanticAnalyzer = RWS (String, ClassEnvironment) [TypeCheckError] ObjectEnvironment

type SemanticAnalyzerM a = MaybeT SemanticAnalyzer a

type ProgramAnalyzer = ReaderT ClassEnvironment (Writer [TypeCheckError])
