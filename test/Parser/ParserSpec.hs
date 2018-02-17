{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Lexer.Token
import Parser.AST
import Parser.ParserUtil
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)
import Data.Maybe (fromJust)

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
        it "should parse a feature" $ do
          let (Attribute featureIdentifier featureType featureExpr) = parseCode "foo : Foo"
          featureIdentifier `shouldBe` Identifier "foo"
          featureType `shouldBe` Type "Foo"
          featureExpr `shouldBe` Nothing
        it "should parse a feature assigned to an expression" $ do
          let (Attribute featureIdentifier featureType featureExpr) = parseCode "foo : Foo <- bar"
          featureIdentifier `shouldBe` Identifier "foo"
          featureType `shouldBe` Type "Foo"
          featureExpr `shouldBe` Just (IdentifierExpr "bar")




testExpression :: String -> Expression -> Expectation
testExpression code expectedExpression =
  let features = parseCode ("foo : Foo <- " ++ code)
  in fromJust (getFeatExpr features) `shouldBe` expectedExpression
