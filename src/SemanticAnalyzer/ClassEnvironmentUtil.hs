{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.ClassEnvironmentUtil where

import SemanticAnalyzer.Class
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import Util

initialClassEnvironment :: ClassEnvironment
initialClassEnvironment =
  [ "Object" =: ObjectClass
  , "String" =:
    ClassRecord
      "String"
      ObjectClass
      (toMap ["length() : Int", "concat(s : String) : String", "substr(i : Int, l : Int)"])
      []
  , "Bool" =: ClassRecord "Bool" ObjectClass [] []
  , "Int" =: ClassRecord "Int" ObjectClass [] []
  , "IO" =: ClassRecord "IO" ObjectClass [] []
  ]

ioRecord :: ClassRecord
ioRecord = ClassRecord "IO" ObjectClass [] []

toClassRecord :: String -> ClassRecord -> [String] -> [String] -> ClassRecord
toClassRecord className' parentRecord methodCodes attributeCodes =
  ClassRecord className' parentRecord (toMap methodCodes) (toMap attributeCodes)
