{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.InitialClassEnvironment where

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
      [ "length" =: MethodRecord "length" [] "Int"
      , "concat" =: MethodRecord "concat" [("s", "String")] "String"
      , "substr" =: MethodRecord "substr" [("i", "Int"), ("l", "Int")] "String"
      ]
      []
  , "Bool" =: ClassRecord "Bool" ObjectClass [] []
  , "Int" =: ClassRecord "Int" ObjectClass [] []
  , "IO" =: ClassRecord "IO" ObjectClass [] []
  ]

ioRecord :: ClassRecord
ioRecord = ClassRecord "IO" ObjectClass [] []
