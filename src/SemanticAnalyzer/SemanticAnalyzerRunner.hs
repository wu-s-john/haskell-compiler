{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzerRunner where

import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer.Lazy (runWriter)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError

runProgramAnalyzer :: ClassEnvironment -> ProgramAnalyzer a -> (a, [SemanticError])
runProgramAnalyzer classEnvironment analyzer = runWriter (runReaderT analyzer classEnvironment)
