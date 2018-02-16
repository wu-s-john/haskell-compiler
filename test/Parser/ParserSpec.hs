{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Lexer.Token
import Parser.AST
import Parser.ParserUtil
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parser" $
  describe "unit-tests" $ do
    it "should parse a binary expression" $
      parseExpression "1 + 2 * 5" `shouldBe`
      BinaryOp PlusOperator (IntegerExpr 1) (BinaryOp TimesOperator (IntegerExpr 2) (IntegerExpr 5))
    it "should parse division" $
      parseExpression "6 / 2" `shouldBe` BinaryOp DivideOperator (IntegerExpr 6) (IntegerExpr 2)
