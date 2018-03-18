{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.SemanticCheckSpec
  ( main
  , spec
  ) where

import Parser.ParserUtil (parse)
import SemanticAnalyzer.SemanticAnalyzer
import SemanticAnalyzer.SemanticCheck (semanticCheck)
import SemanticAnalyzer.TypedAST (ExpressionT(..), LetBindingT(..))
import SemanticAnalyzer.Util
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Semantic Analysis" $ do
    describe " binary arithmetic" $ do
      it "should annotate correctly a plus operator" $
        testAnalyzer' [] [] "1 + 2" (PlusExprT (IntegerExprT 1) (IntegerExprT 2), [])
      it "should parse an error of a plus operator" $
        testAnalyzer'
          []
          []
          "\"string\" + 2"
          (PlusExprT (StringExprT "string") (IntegerExprT 2), [NonIntArgumentsPlus "String" "Int"])
    describe "identifier" $ do
      it "should find the type of a variable" $
        testAnalyzer' [] ["foo" =: "Foo"] "foo" (IdentifierExprT "foo" "Foo", [])
      it "should throw an error when identifier is not in the object environment" $
        testAnalyzer' [] [] "foo" (IdentifierExprT "foo" "Object", [UndeclaredIdentifier "foo"])
    describe "new" $ do
      it "should annotate a new expression for a regular type" $ testAnalyzer "Foo" classEnvironmentMock [] "new Foo" (NewExprT "Foo" "Foo", [])
      it "should annotate a new expression for SELF_TYPE" $
        testAnalyzer "Foo" classEnvironmentMock [] "new SELF_TYPE" (NewExprT "SELF_TYPE" "SELF_TYPE", [])
      it "should annotate a new expression for SELF_TYPE" $
        testAnalyzer "Foo" [] [] "new Bar" (NewExprT "Bar" "Object", [UndefinedNewType "Bar"])
    describe "let expression" $
      describe "letBindingT" $ do
        describe "initial expression is a subtype of it's declared variable" $ do
          it "declared variable is not initialized but is used properly" $
            testAnalyzer'
              classEnvironmentMock
              []
              "let x : Int in x + 5"
              (LetExprT $ LetBindingT "x" "Int" Nothing (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 5)), [])
          it "declared variable is used properly" $
            testAnalyzer'
              classEnvironmentMock
              []
              "let x : Int <- 4 in x + 5"
              ( LetExprT $
                LetBindingT "x" "Int" (Just $ IntegerExprT 4) (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 5))
              , [])
          it "declared variable is not used" $
            testAnalyzer'
              classEnvironmentMock
              []
              "let x : Int <- 4 in 5"
              (LetExprT $ LetBindingT "x" "Int" (Just $ IntegerExprT 4) (IntegerExprT 5), [])
          it "overrides variable that was previously declared" $
            testAnalyzer'
              classEnvironmentMock
              ["x" =: "String"]
              "let x : Int <- 4 in x"
              (LetExprT $ LetBindingT "x" "Int" (Just $ IntegerExprT 4) (IdentifierExprT "x" "Int"), [])
        describe "expression is not a subtype of it's declared variable" $
          it "declared variable is still follows it's typing" $
          testAnalyzer'
            classEnvironmentMock
            []
            "let x : Int <- \"Hello World\" in x + 5"
            ( LetExprT $
              LetBindingT
                "x"
                "Int"
                (Just $ StringExprT "Hello World")
                (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 5))
            , [MismatchDeclarationType "String" "Int"])
    describe "method dispatch" $ do
      it "should throw an error if a method could not be found for a class" $
        testAnalyzer'
          classEnvironmentMock
          []
          "foo()"
          (MethodDispatchT SelfVarExprT "foo" [] "Object", [UndefinedMethod "foo"])
      it "should throw an error if the caller expression returns an undefined class" $
        testAnalyzer'
          []
          ["baz" =: "Baz"]
          "baz.foo()"
          (MethodDispatchT (IdentifierExprT "baz" "Baz") "foo" [] "Object", [DispatchUndefinedClass "Baz"])
      it "should parse if it can call a valid method in a valid class with no parameters" $
        testAnalyzer' classEnvironmentMock [] "call8()" (MethodDispatchT SelfVarExprT "call8" [] "Int", [])
      it "should throw an error if the number of parameters do not match" $
        testAnalyzer'
          classEnvironmentMock
          []
          "call8(8)"
          (MethodDispatchT SelfVarExprT "call8" [IntegerExprT 8] "Int", [WrongNumberParameters "call8"])
      it "should throw an error if the a parameter is not a subtype of it's argument" $
        testAnalyzer'
          classEnvironmentMock
          []
          "sum(\"string\", 2)"
          ( MethodDispatchT SelfVarExprT "sum" [StringExprT "string", IntegerExprT 2] "Int"
          , [WrongParameterType "sum" "a" "Int" "String"])
  where
    testAnalyzer currentClassName classEnvironment objectEnvironment sourceCode result =
      applyParameters currentClassName classEnvironment objectEnvironment (semanticCheck (parse sourceCode)) `shouldBe`
      result
    testAnalyzer' = testAnalyzer ""
