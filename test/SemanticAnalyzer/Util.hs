{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.Util where

import qualified Data.Map as M
import qualified Parser.TerminalNode as T

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (evalState)
import Control.Monad.Writer (runWriterT)
import qualified SemanticAnalyzer.Class as Class
import SemanticAnalyzer.InitialClassEnvironment
import SemanticAnalyzer.SemanticAnalyzer
import Util

fooClassRecord :: Class.ClassRecord
fooClassRecord =
  Class.ClassRecord
    "Foo"
    Class.ObjectClass
    [ "call8" =: Class.MethodRecord "call8" [] "Int"
    , "sum" =: Class.MethodRecord "sum" [("a", "Int"), ("b", "Int")] "Int"
    ]
    []

classEnvironmentMock :: Class.ClassEnvironment
classEnvironmentMock =
  initialClassEnvironment `M.union` ["Foo" =: fooClassRecord, "Bar" =: Class.ClassRecord "Bar" fooClassRecord [] []]

classEnvironmentWithInheritedBasicClass :: Class.ClassEnvironment
classEnvironmentWithInheritedBasicClass =
  classEnvironmentMock `M.union` ["Baz" =: Class.ClassRecord "Baz" intRecord [] []]

applyParameters :: T.Type -> Class.ClassEnvironment -> ObjectEnvironment -> SemanticAnalyzer a -> (a, [SemanticError])
applyParameters currentClassName classEnvironment objectEnvironment semanticAnalyzer =
  evalState (runWriterT (runReaderT semanticAnalyzer (currentClassName, classEnvironment))) objectEnvironment
