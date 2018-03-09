{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}

module SemanticAnalyzer.TypedASTSpec
  ( main
  , spec
  ) where

import Control.Monad.State (evalState)
import Control.Monad.Writer (runWriter, runWriterT)
import Parser.ParserUtil (parseExpression)
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), SemanticError(..), semanticCheck)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

spec :: Spec
spec =
  describe "Semantic Analysis" $ do
    describe " binary arithmetic" $ do
      it "should annotate correctly a plus operator" $
        testAnalyzer [] "1 + 2" (PlusExprT (IntegerExprT 1) (IntegerExprT 2), [])
      it "should parse an error of a plus operator" $
        testAnalyzer
          []
          "\"string\" + 2"
          (PlusExprT (StringExprT "string") (IntegerExprT 2), [NonIntArgumentsPlus "String" "Int"])
    describe "identifier" $ do
      it "should find the type of a variable" $ testAnalyzer ["foo" =: "Foo"] "foo" (IdentifierExprT "foo" "Foo", [])
      it "should throw an error when identifier is not in the object environment" $
        testAnalyzer [] "foo" (IdentifierExprT "foo" "Object", [UndeclaredIdentifier "foo"])
  where
    testAnalyzer objectEnvironment sourceCode result =
      evalState (runWriterT (semanticCheck (parseExpression sourceCode))) objectEnvironment `shouldBe` result
