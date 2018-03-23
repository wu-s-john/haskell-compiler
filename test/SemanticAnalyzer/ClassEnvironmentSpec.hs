{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.ClassEnvironmentSpec
  ( main
  , spec
  ) where

import Control.Monad.Writer (runWriter)
import qualified Data.Map as M
import Data.String (fromString)
import qualified Parser.AST as AST
import Parser.ParserUtil (parse)
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class (ClassRecord(..), toMap)
import SemanticAnalyzer.ClassChecker (ClassInheritanceGraph)
import SemanticAnalyzer.ClassEnvironment
       (ClassEnvironment, createEnvironment, mergeAttributes,
        mergeMethods)
import SemanticAnalyzer.ClassEnvironmentUtil (toClassRecord)
import SemanticAnalyzer.InheritanceFeatureError
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ClassEnvironment" $ do
    describe "createClassEnvironment" $ do
      it "should be able to parse a program with a class" $
        createEnvironment M.empty (AST.Program []) `shouldBe` return M.empty
      it "should be able to parse a program with an orphaned class" $
        testCreateEnvironment
          [("Foo", "Object")]
          "class Foo {x : X; twice(num: Int): Int {2 * x};};"
          (["Foo" =: fooClassRecord], [])
    describe "mergeAttributes" $ do
      it "should not produce errors if a class does not inherit attributes that it's parent has" $
        testAttribute ["x : X"] ["y : Y"] (["x : X", "y : Y"], [])
      it "should produce errors if a class has attributes that it's parent has" $
        testAttribute ["x : X", "y : X"] ["y : Y"] (["x : X", "y : X"], [RedefinedAttribute "y" (fromString "Foo")])
    describe "mergeMethods" $ do
      it "should not report errors for methods if a class does not inherit methods that it's parent has" $
        testMethod ["foo(x : X, y : Y) : Z"] [] (["foo(x : X, y : Y) : Z"], [])
      it "should report errrors for methods if a class does inherit a method with no matching return types" $ -- todo test for inheritance
        testMethod
          ["foo(x : X) : Z"]
          ["foo(x : X) : Bar"]
          (["foo(x : X) : Z"], [DifferentMethodReturnType (fromString "Z") (fromString "Bar")])
  where
    testCreateEnvironment ::
         ClassInheritanceGraph -> String -> (ClassEnvironment, [InheritanceFeatureError]) -> Expectation
    testCreateEnvironment classGraph sourceCode expectedResult =
      runWriter (createEnvironment classGraph (parse sourceCode)) `shouldBe` expectedResult
    testAttribute :: [String] -> [String] -> ([String], [InheritanceFeatureError]) -> Expectation
    testAttribute classAttr parentAttr (expectedMapString, errors) =
      runWriter (mergeAttributes (fromString "Foo") (toMap classAttr) (toMap parentAttr)) `shouldBe`
      (toMap expectedMapString, errors)
    testMethod :: [String] -> [String] -> ([String], [InheritanceFeatureError]) -> Expectation
    testMethod classStrings parentStrings (expectedMapString, errors) =
      runWriter (mergeMethods (toMap classStrings) (toMap parentStrings)) `shouldBe` (toMap expectedMapString, errors)

--      it "should be able to retrieve attributes from it's parent class given that the class is empty" $
--        testCreateEnvironment
--          [("Bar", "Foo"), ("Foo", "Object")]
--          "class Foo {x : X; twice(num: Int): Int {2 * x};}; class Bar inherits Foo {};"
--          (["Bar" =: fooFeatures "Bar" fooClassRecord, "Foo" =: fooClassRecord], [])
--      it "should be able to recieve attributes from it's parent class and have it's own parameters" $
--        testCreateEnvironment
--          [("Bar", "Foo"), ("Foo", "Object")]
--          "class Foo {x : X; twice(num: Int): Int {2 * x};}; class Bar inherits Foo {y : Y;};"
--          (["Bar" =: toClassRecord "Bar" fooClassRecord fooMethods ["x : X", "y : Y"], "Foo" =: fooClassRecord], [])
fooFeatures :: T.Identifier -> ClassRecord -> ClassRecord
fooFeatures name parent' = toClassRecord name parent' fooMethods ["x : X"]

fooMethods :: [String]
fooMethods = ["twice(num : Int) : Int"]

fooClassRecord :: ClassRecord
fooClassRecord = fooFeatures "Foo" ObjectClass
