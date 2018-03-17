{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.Util where

import qualified Data.Map as M
import qualified Parser.TerminalNode as T

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState)
import Control.Monad.Writer (runWriterT)
import SemanticAnalyzer.Class (ClassRecord(..), MethodRecord(..))
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.InitialClassEnvironment
import SemanticAnalyzer.SemanticAnalyzer
import Util

fooClassRecord :: ClassRecord
fooClassRecord =
  ClassRecord
    "Foo"
    ObjectClass
    ["call8" =: MethodRecord "call8" [] "Int", "sum" =: MethodRecord "sum" [("a", "Int"), ("b", "Int")] "Int"]
    []

classEnvironmentMock :: ClassEnvironment
classEnvironmentMock =
  initialClassEnvironment `M.union` ["Foo" =: fooClassRecord, "Bar" =: ClassRecord "Bar" fooClassRecord [] []]

classEnvironmentWithInheritedBasicClass :: ClassEnvironment
classEnvironmentWithInheritedBasicClass = classEnvironmentMock `M.union` ["Baz" =: ClassRecord "Baz" intRecord [] []]

applyParameters :: T.Type -> ClassEnvironment -> ObjectEnvironment -> SemanticAnalyzer a -> (a, [SemanticError])
applyParameters currentClassName classEnvironment objectEnvironment semanticAnalyzer =
  evalState (runWriterT (runReaderT semanticAnalyzer (currentClassName, classEnvironment))) objectEnvironment
