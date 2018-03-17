{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.ClassSpec
  ( main
  , spec
  ) where

import Control.Monad.Writer (runWriter)
import qualified Data.Map as M
import Data.Map ()
import qualified Parser.AST as AST
import Parser.ParserUtil (parse)
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (AttributeRecord(..), ClassRecord(..), InheritanceErrors(..),
        MethodMap, MethodRecord(..), createEnvironment, mergeAttributes,
        mergeMethods, FeatureTransformer,toRecord)
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)
import Util

main :: IO ()
main = hspec spec

fooFeatures :: T.Identifier -> ClassRecord -> ClassRecord
fooFeatures name parent' = ClassRecord name parent' fooMethods ["x" =: AttributeRecord "x" "X"]

fooMethods :: MethodMap
fooMethods = ["twice" =: MethodRecord "twice" [("num", "Int")] "Int"]

fooClassRecord :: ClassRecord
fooClassRecord = fooFeatures "Foo" ObjectClass

spec :: Spec
spec =
  describe "class" $ do
    describe "method" $ do
      it "should not parse an attribute" $ "foo : Bar" |-> (Nothing :: Maybe MethodRecord)
      it "should parse a method with no parameters" $
        "foo () : Bar {true}" |-> (Just $ MethodRecord "foo" [] "Bar")
      it "should parse a method with parameters" $
        "foo (x : X, y: Y) : Foo {true}" |-> (Just $ MethodRecord "foo" [("x", "X"), ("y", "Y")] "Foo")
    describe "attribute" $ do
      it "should not parse a method" $ "foo() : Bar {true}" |-> (Nothing :: Maybe AttributeRecord)
      it "should parse an attribute" $ "foo : Bar" |-> (Just $ AttributeRecord "foo" "Bar")
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
        [ "Bar" =:
          ClassRecord "Bar" fooClassRecord fooMethods ["x" =: AttributeRecord "x" "X", "y" =: AttributeRecord "y" "Y"]
        , "Foo" =: fooClassRecord
        ]
    describe "mergeAttributes" $ do
      it "should not produce errors if a class does not inherit attributes that it's parent has" $
        testAttribute
          "Foo"
          ["x" =: AttributeRecord "x" "X"]
          ["y" =: AttributeRecord "y" "Y"]
          (["x" =: AttributeRecord "x" "X", "y" =: AttributeRecord "y" "Y"], [])
      it "should produce errors if a class has attributes that it's parent has" $
        testAttribute
          "Foo"
          ["x" =: AttributeRecord "x" "X", "y" =: AttributeRecord "y" "X"]
          ["y" =: AttributeRecord "y" "Y"]
          (["x" =: AttributeRecord "x" "X", "y" =: AttributeRecord "y" "X"], [RedefinedAttribute "y" "Foo"])
    describe "mergeMethods" $ do
      it "should not report errors for methods if a class does not inherit methods that it's parent has" $
        testMethod
          ["foo" =: MethodRecord "foo" [("x", "X"), ("y", "Y")] "Z"]
          []
          (["foo" =: MethodRecord "foo" [("x", "X"), ("y", "Y")] "Z"], [])
      it "should report errrors for methods if a class does inherit a method with no matching return types" $ -- todo test for inheritance
        testMethod
          ["foo" =: MethodRecord "foo" [("x", "X")] "Z"]
          ["foo" =: MethodRecord "foo" [("x", "X")] "Bar"]
          (["foo" =: MethodRecord "foo" [("x", "X")] "Z"], [DifferentMethodReturnType "Z" "Bar"])
  where
    testAttribute className' classAttr parentAttr expectedResult =
      runWriter (mergeAttributes className' classAttr parentAttr) `shouldBe` expectedResult
    testMethod classMethods parentMethods expectedResult =
      runWriter (mergeMethods classMethods parentMethods) `shouldBe` expectedResult

(|->) :: FeatureTransformer a => Show a => Eq a=> String -> Maybe a -> Expectation
code |-> expected = toRecord (parse code) `shouldBe` expected
