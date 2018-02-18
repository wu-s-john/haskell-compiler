{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Data.Maybe (fromJust)
import Lexer.Token
import Parser.AST
import Parser.ParserUtil
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parser" $
  describe "unit-tests" $ do
    describe "expressions" $ do
      it "should parse a binary expression" $
        "1 + 2 * 5" `testExpression`
        BinaryOp PlusOperator (IntegerExpr 1) (BinaryOp TimesOperator (IntegerExpr 2) (IntegerExpr 5))
      it "should parse division" $ "6 / 2" `testExpression` BinaryOp DivideOperator (IntegerExpr 6) (IntegerExpr 2)
      it "should parse an identifier expression" $ "foo" `testExpression` IdentifierExpr "foo"
    describe "features" $ do
      it "should parse a feature" $ testFeature "foo : Foo" $ Attribute (Identifier "foo") (Type "Foo") Nothing
      it "should parse a feature assigned to an expression" $
        testFeature "foo : Foo <- bar" $ Attribute (Identifier "foo") (Type "Foo") (Just (IdentifierExpr "bar"))
    describe "class" $ do
      it "should parse an orphaned class" $ parseCode "class Foo {\n\n}\n" `shouldBe` OrphanedClass (Type "Foo") []
      it "should parse an inherited class" $
        parseCode "class Foo inherits Bar {\n\n}\n" `shouldBe` InheritedClass (Type "Foo") (Type "Bar") []

parseFeature :: String -> Feature
parseFeature code =
  let classNode = parseCode $ "class Foo {\n" ++ code ++ ";\n}\n"
  in head $ getFeatures classNode

parseExpression :: String -> Expression
parseExpression code =
  let featureNode = parseFeature $ "foo : Foo <- " ++ code
  in fromJust (getFeatExpr featureNode)

testParser ::
     Show a
  => Eq a =>
       (String -> a) -> String -> a -> Expectation
testParser parser code expectedResult = parser code `shouldBe` expectedResult

testFeature :: String -> Feature -> Expectation
testFeature = testParser parseFeature

testExpression :: String -> Expression -> Expectation
testExpression = testParser parseExpression
