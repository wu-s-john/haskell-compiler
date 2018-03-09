{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.TypedASTSpec
  ( main
  , spec
  ) where

import Control.Monad.Writer (runWriter)
import Parser.AST as AST
import Parser.ParserUtil (parseExpression)
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), SemanticError(..), semanticAnalyzer)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Semantic Analysis" $
  describe " binary arithmetic" $ do
    it "should annotate correctly a plus operator" $
      "1 + 2" `testAnalyzer` (PlusExprT (IntegerExprT 1) (IntegerExprT 2), [])
    it "should parse an error of a plus operator" $
      "\"string\" + 2" `testAnalyzer`
      (PlusExprT (StringExprT "string") (IntegerExprT 2), [NonIntArgumentsPlus "String" "Int"])
  where
    testAnalyzer sourceCode result = runWriter (semanticAnalyzer (parseExpression sourceCode)) `shouldBe` result
