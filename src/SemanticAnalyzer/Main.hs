{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.Main where

import Control.Arrow (left)
import Control.Monad.Reader (runReaderT)
import Control.Monad.Writer (runWriter)
import Parser.AST (Program)
import SemanticAnalyzer.ClassChecker (checkAndVerifyClassGraph)
import SemanticAnalyzer.ClassEnvironment (createEnvironment)
import SemanticAnalyzer.ClassRelationshipError
       (ClassRelationshipError)
import SemanticAnalyzer.InheritanceFeatureError
import SemanticAnalyzer.SemanticCheck (semanticCheck)
import SemanticAnalyzer.TypeCheckError (TypeCheckError)
import SemanticAnalyzer.TypedAST (ProgramT)

-- These errors occur sequentially
data SemanticError
  = Phase1 [ClassRelationshipError]
  | Phase2 [Either InheritanceFeatureError TypeCheckError]

runSemanticAnalyzer :: Program -> Either SemanticError ProgramT
runSemanticAnalyzer program = do
  inheritanceGraph <- left Phase1 $ checkAndVerifyClassGraph program
  let (classEnvironment, inheritanceErrors) = runWriter $ createEnvironment inheritanceGraph program
  let (programT, typeCheckErrors) = runWriter $ runReaderT (semanticCheck program) classEnvironment
  case map Left inheritanceErrors ++ map Right typeCheckErrors of
    [] -> Right programT
    phase2Errors -> Left $ Phase2 phase2Errors
