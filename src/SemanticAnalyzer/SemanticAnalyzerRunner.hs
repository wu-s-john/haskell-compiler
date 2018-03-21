{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzerRunner where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState)
import Control.Monad.Writer (runWriterT,runWriter)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticError


runAnalyzer :: String -> ClassEnvironment -> ObjectEnvironment -> SemanticAnalyzer a -> (a, [SemanticError])
runAnalyzer classType classEnvironment objectEnvironment semanticAnalyzer =
  evalState (runWriterT (runReaderT semanticAnalyzer (classType, classEnvironment))) objectEnvironment

runProgramAnalyzer :: ClassEnvironment -> ProgramAnalyzer a -> (a, [SemanticError])
runProgramAnalyzer classEnvironment analyzer = runWriter (runReaderT analyzer classEnvironment)
