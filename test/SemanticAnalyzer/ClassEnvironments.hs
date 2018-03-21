{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.ClassEnvironments where

import qualified Data.Map as M

import SemanticAnalyzer.Class
       (AttributeRecord(..), ClassRecord(..), MethodRecord(..))
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.InitialClassEnvironment
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
  [ "Foo" =: fooClassRecord
  , "Bar" =: ClassRecord "Bar" fooClassRecord [] []
  , "Quux" =: ClassRecord "Quux" ObjectClass [] []
  ]

classErrorMock :: M.Map String ClassRecord
classErrorMock =
  initialClassEnvironment `M.union`
  [ "MultipleErrors" =:
    ClassRecord
      "MultipleErrors"
      ObjectClass
      ["add8" =: MethodRecord "add8" [] "Int"]
      ["eight" =: AttributeRecord "eight" "Int"]
  ]
