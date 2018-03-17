{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.ClassSpec
  ( main
  , spec
  ) where

import Parser.ParserUtil (parse)
import SemanticAnalyzer.Class (AttributeRecord(..), FeatureTransformer, MethodRecord(..), toRecord)
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec


spec :: Spec
spec =
  describe "class" $ do
    describe "method" $ do
      it "should not parse an attribute" $ "foo : Bar" |-> (Nothing :: Maybe MethodRecord)
      it "should parse a method with no parameters" $ "foo () : Bar {true}" |-> (Just $ MethodRecord "foo" [] "Bar")
      it "should parse a method with parameters" $
        "foo (x : X, y: Y) : Foo {true}" |-> (Just $ MethodRecord "foo" [("x", "X"), ("y", "Y")] "Foo")
    describe "attribute" $ do
      it "should not parse a method" $ "foo() : Bar {true}" |-> (Nothing :: Maybe AttributeRecord)
      it "should parse an attribute" $ "foo : Bar" |-> (Just $ AttributeRecord "foo" "Bar")


(|->) ::
     FeatureTransformer a
  => Show a =>
       Eq a =>
         String -> Maybe a -> Expectation
code |-> expected = toRecord (parse code) `shouldBe` expected
