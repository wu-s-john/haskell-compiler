{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.TypedASTSpec
  ( main
  , spec
  ) where

import Control.Monad.Reader (Reader, runReader)
import Control.Monad.State (evalState)
import Control.Monad.Writer (runWriterT)
import Parser.ParserUtil (parseExpression)
import SemanticAnalyzer.Class (ClassEnvironment, ClassRecord(..))
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), SemanticError(..), (<==), semanticCheck,(\/))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Parser.TerminalNode as T

main :: IO ()
main = hspec spec

(=:) :: a -> b -> (a, b)
a =: b = (a, b)

fooClassRecord :: ClassRecord
fooClassRecord = ClassRecord "Foo" ObjectClass [] []

classEnvironment :: ClassEnvironment
classEnvironment = ["Foo" =: fooClassRecord, "Bar" =: ClassRecord "Bar" fooClassRecord [] []]

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
    describe "isSubtype" $ do
      it "should return true if two types are subtypes of each other" $ testSubtype classEnvironment "Bar" "Foo" True
      it "should return false if two types are not subtypes of each other" $
        testSubtype classEnvironment "Foo" "Bar" False
      it "should return false if the possible subtype is undefined" $
        testSubtype classEnvironment "X" "Foo" False
      it "should return false if the parent is undefined" $
        testSubtype classEnvironment "Foo" "X" False
      it "should return true if the parent subtype is Object" $ testSubtype classEnvironment "Foo" "Object" True
    describe "lub (lowest upper bound)" $ do
      it "should return object if two types do not share the same ancestors" $
        testUpperBound classEnvironment "Foo" "X" "Object"
      it "should compute the lub of two types that share the same ancestors" $
        testUpperBound classEnvironment "Foo" "Foo" "Foo"
  where
    testAnalyzer objectEnvironment sourceCode result =
      evalState (runWriterT (semanticCheck (parseExpression sourceCode))) objectEnvironment `shouldBe` result
    testSubtype classEnvironment' possibleSubType parentType result =
      runReader ((possibleSubType <== parentType) :: Reader ClassEnvironment Bool) classEnvironment' `shouldBe` result
    testUpperBound classEnvironment' possibleSubType parentType result =
      runReader ((possibleSubType \/ parentType) :: Reader ClassEnvironment T.Type) classEnvironment' `shouldBe` result
