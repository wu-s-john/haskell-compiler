{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.Util where

import qualified Data.Map as M

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
  initialClassEnvironment `M.union`
  ["Foo" =: fooClassRecord, "Bar" =: ClassRecord "Bar" fooClassRecord [] [], "Quux" =: ClassRecord "Quux" ObjectClass [] []]

applyParameters :: String -> ClassEnvironment -> ObjectEnvironment -> SemanticAnalyzer a -> (a, [SemanticError])
applyParameters classType classEnvironment objectEnvironment semanticAnalyzer =
  evalState (runWriterT (runReaderT semanticAnalyzer (classType, classEnvironment))) objectEnvironment
