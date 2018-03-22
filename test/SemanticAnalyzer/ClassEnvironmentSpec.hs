{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module SemanticAnalyzer.ClassEnvironmentSpec
  ( main
  , spec
  ) where

import qualified Data.Map as M

import qualified Parser.AST as AST
import qualified Parser.TerminalNode as T

import Parser.ParserUtil (parse)
import Util

import SemanticAnalyzer.ClassEnvironment
       (InheritanceErrors(..), createEnvironment, mergeAttributes,
        mergeMethods)

import Control.Monad.Writer (runWriter)
import Data.String (fromString)
import SemanticAnalyzer.Class (ClassRecord(..), toMap)
import SemanticAnalyzer.ClassEnvironmentUtil (toClassRecord)
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "ClassEnvironment" $ do
    describe "createClassEnvironment" $ do
      it "should be able to parse a program with a class" $
        createEnvironment M.empty (AST.Program []) `shouldBe` M.empty
      it "should be able to parse a program with an orphaned class" $
        createEnvironment [("Foo", "Object")] (parse "class Foo {x : X; twice(num: Int): Int {2 * x};};") `shouldBe`
        ["Foo" =: fooClassRecord]
      it "should be able to retrieve attributes from it's parent class" $
        createEnvironment
          [("Bar", "Foo"), ("Foo", "Object")]
          (parse "class Foo {x : X; twice(num: Int): Int {2 * x};}; class Bar inherits Bar {};") `shouldBe`
        ["Bar" =: fooFeatures "Bar" fooClassRecord, "Foo" =: fooClassRecord]
      it "should be able to extends attributes from it's parent class" $
        createEnvironment
          [("Bar", "Foo"), ("Foo", "Object")]
          (parse "class Foo {x : X; twice(num: Int): Int {2 * x};}; class Bar inherits Foo {y : Y;};") `shouldBe`
        ["Bar" =: toClassRecord "Bar" fooClassRecord fooMethods ["x : X", "y : Y"], "Foo" =: fooClassRecord]
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
    testAttribute :: [String] -> [String] -> ([String], [InheritanceErrors]) -> Expectation
    testAttribute classAttr parentAttr (expectedMapString, errors) =
      runWriter (mergeAttributes (fromString "Foo") (toMap classAttr) (toMap parentAttr)) `shouldBe`
      (toMap expectedMapString, errors)
    testMethod :: [String] -> [String] -> ([String], [InheritanceErrors]) -> Expectation
    testMethod classStrings parentStrings (expectedMapString, errors) =
      runWriter (mergeMethods (toMap classStrings) (toMap parentStrings)) `shouldBe` (toMap expectedMapString, errors)

fooFeatures :: T.Identifier -> ClassRecord -> ClassRecord
fooFeatures name parent' = toClassRecord name parent' fooMethods ["x : X"]

fooMethods :: [String]
fooMethods = ["twice(num : Int) : Int"]

fooClassRecord :: ClassRecord
fooClassRecord = fooFeatures "Foo" ObjectClass
