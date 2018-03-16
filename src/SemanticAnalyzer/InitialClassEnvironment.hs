{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.InitialClassEnvironment where

import SemanticAnalyzer.Class
import Util

initialClassEnvironment :: ClassEnvironment
initialClassEnvironment =
  [ "Object" =: ObjectClass
  , "String" =:
    BasicClass
      "String"
      [ "length" =: MethodRecord "length" [] "Int"
      , "concat" =: MethodRecord "concat" [("s", "String")] "String"
      , "substr" =: MethodRecord "substr" [("i", "Int"), ("l", "Int")] "String"
      ]
  , "Bool" =: BasicClass "Bool" []
  , "Int" =: intRecord
  , "IO" =: BasicClass "IO" []
  ]

intRecord :: ClassRecord
intRecord = BasicClass "Int" []
