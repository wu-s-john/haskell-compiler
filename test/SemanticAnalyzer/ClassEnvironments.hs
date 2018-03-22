{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.ClassEnvironments where

import qualified Data.Map as M

import SemanticAnalyzer.Class (ClassRecord(..), toMap)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.ClassEnvironmentUtil
import Util

fooClassRecord :: ClassRecord
fooClassRecord = toClassRecord "Foo" ObjectClass ["call8() : Int", "sum(a : Int, b : Int): Int"] []

classEnvironmentMock :: ClassEnvironment
classEnvironmentMock =
  initialClassEnvironment `M.union`
  [ "Foo" =: fooClassRecord
  , "Bar" =: toClassRecord "Bar" fooClassRecord [] []
  , "Quux" =: toClassRecord "Quux" ObjectClass [] []
  ]

classErrorMock :: M.Map String ClassRecord
classErrorMock =
  initialClassEnvironment `M.union`
  ["MultipleErrors" =: toClassRecord "MultipleErrors" ObjectClass ["add8() : Int"] ["eight : Int"]]
