{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.TypedASTSpec
  ( main
  , spec
  ) where

import Control.Monad.Reader (Reader, runReader, runReaderT)
import Control.Monad.State (evalState, get)
import Control.Monad.Writer (runWriterT)
import Data.Map as M
import Parser.ParserUtil (parseExpression)
import Parser.TerminalNode as T
import SemanticAnalyzer.Class
       (ClassEnvironment, ClassRecord(..), MethodRecord(..))
import SemanticAnalyzer.InitialClassEnvironment
import SemanticAnalyzer.TypedAST
       (ExpressionT(..), LetBindingT(..), ObjectEnvironment,
        SemanticAnalyzer, SemanticError(..), (/>), (<==), (\/),
        semanticCheck)
import Test.Hspec (Spec, describe, hspec, it, shouldBe)
import Util

main :: IO ()
main = hspec spec

fooClassRecord :: ClassRecord
fooClassRecord =
  ClassRecord
    "Foo"
    ObjectClass
    ["call8" =: MethodRecord "call8" [] "Int", "sum" =: MethodRecord "sum" [("a", "Int"), ("b", "Int")] "Int"]
    []

classEnvironment :: ClassEnvironment
classEnvironment =
  initialClassEnvironment `M.union` ["Foo" =: fooClassRecord, "Bar" =: ClassRecord "Bar" fooClassRecord [] []]

classEnvironmentWithInheritedBasicClass :: ClassEnvironment
classEnvironmentWithInheritedBasicClass = classEnvironment `M.union` ["Baz" =: ClassRecord "Baz" intRecord [] []]

spec :: Spec
spec =
  describe "Semantic Analysis" $ do
    describe " binary arithmetic" $ do
      it "should annotate correctly a plus operator" $
        testAnalyzer [] [] "1 + 2" (PlusExprT (IntegerExprT 1) (IntegerExprT 2), [])
      it "should parse an error of a plus operator" $
        testAnalyzer
          []
          []
          "\"string\" + 2"
          (PlusExprT (StringExprT "string") (IntegerExprT 2), [NonIntArgumentsPlus "String" "Int"])
    describe "identifier" $ do
      it "should find the type of a variable" $ testAnalyzer [] ["foo" =: "Foo"] "foo" (IdentifierExprT "foo" "Foo", [])
      it "should throw an error when identifier is not in the object environment" $
        testAnalyzer [] [] "foo" (IdentifierExprT "foo" "Object", [UndeclaredIdentifier "foo"])
    describe "let expression" $
      describe "letBindingT" $ do
        describe "initial expression is a subtype of it's declared variable" $ do
          it "declared variable is not initialized but is used properly" $
            testAnalyzer
              classEnvironment
              []
              "let x : Int in x + 5"
              (LetExprT $ LetBindingT "x" "Int" Nothing (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 5)), [])
          it "declared variable is used properly" $
            testAnalyzer
              classEnvironment
              []
              "let x : Int <- 4 in x + 5"
              ( LetExprT $
                LetBindingT "x" "Int" (Just $ IntegerExprT 4) (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 5))
              , [])
          it "declared variable is not used" $
            testAnalyzer
              classEnvironment
              []
              "let x : Int <- 4 in 5"
              (LetExprT $ LetBindingT "x" "Int" (Just $ IntegerExprT 4) (IntegerExprT 5), [])
          it "overrides variable that was previously declared" $
            testAnalyzer
              classEnvironment
              ["x" =: "String"]
              "let x : Int <- 4 in x"
              (LetExprT $ LetBindingT "x" "Int" (Just $ IntegerExprT 4) (IdentifierExprT "x" "Int"), [])
        describe "expression is not a subtype of it's declared variable" $
          it "declared variable is still follows it's typing" $
          testAnalyzer
            classEnvironment
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
        testAnalyzer
          classEnvironment
          []
          "foo()"
          (MethodDispatchT SelfVarExprT "foo" [] "Object", [UndefinedMethod "foo"])
      it "should throw an error if the caller expression returns an undefined class" $
        testAnalyzer
          []
          ["baz" =: "Baz"]
          "baz.foo()"
          (MethodDispatchT (IdentifierExprT "baz" "Baz") "foo" [] "Object", [DispatchUndefinedClass "Baz"])
      it "should parse if it can call a valid method in a valid class with no parameters" $
        testAnalyzer classEnvironment [] "call8()" (MethodDispatchT SelfVarExprT "call8" [] "Int", [])
      it "should throw an error if the number of parameters do not match" $
        testAnalyzer
          classEnvironment
          []
          "call8(8)"
          (MethodDispatchT SelfVarExprT "call8" [IntegerExprT 8] "Int", [WrongNumberParameters "call8"])
      it "should throw an error if the a parameter is not a subtype of it's argument" $
        testAnalyzer
          classEnvironment
          []
          "sum(\"string\", 2)"
          ( MethodDispatchT SelfVarExprT "sum" [StringExprT "string", IntegerExprT 2] "Int"
          , [WrongParameterType "sum" "a" "Int" "String"])
    describe "/>" $
      it "should set a new variable and is a pplied only to an inner scope" $
      fst $
      applyParameters [] [] $ do
        _ <- ("x", "Int") /> (get >>= (\objectEnvironment -> return $ "x" `M.member` objectEnvironment `shouldBe` True))
        objectEnvironment <- get
        return $ "x" `M.member` objectEnvironment `shouldBe` False
    describe "isSubtype" $ -- todo separate the cases for (String and Int) with IO
     do
      it "a basic class should be a subtype of object" $ testSubtype classEnvironment "String" "Object" True
      it "a basic class should not be a subtype of any constructed type" $
        testSubtype classEnvironment "String" "Foo" False
      it "a basic class should be a subtype of a basic class if they are the same" $
        testSubtype classEnvironment "String" "String" True
      it "a basic class should not be a subtype of a basic class if they are not the same" $
        testSubtype classEnvironment "String" "Int" False
      it "a class record should be a subtype of a basic class only if it inherits the class" $
        testSubtype classEnvironmentWithInheritedBasicClass "Baz" "Int" True
      it "a class record should not be a subtype of a basic class if it does not inherit the class" $
        testSubtype classEnvironment "Baz" "Int" False
      it "should return true if two types are subtypes of each other" $ testSubtype classEnvironment "Bar" "Foo" True
      it "should return false if two types are not subtypes of each other" $
        testSubtype classEnvironment "Foo" "Bar" False
      it "should return false if the possible subtype is undefined" $ testSubtype classEnvironment "X" "Foo" False
      it "should return false if the parent is undefined" $ testSubtype classEnvironment "Foo" "X" False
      it "should return true if the parent subtype is Object" $ testSubtype classEnvironment "Foo" "Object" True
    describe "lub (lowest upper bound)" $ do
      it "should return object if two types do not share the same ancestors" $
        testUpperBound classEnvironment "Foo" "X" "Object"
      it "should compute the lub of two types that share the same ancestors" $
        testUpperBound classEnvironment "Foo" "Foo" "Foo"
      it "should have the lub of a basic class and an object that doesn't inherit from a basic class be an Object" $
        testUpperBound classEnvironment "Foo" "Int" "Object"
      it "should have the lub of a basic class and an object that inherits from a basic class be the basic class" $
        testUpperBound classEnvironmentWithInheritedBasicClass "Baz" "Int" "Int"
  where
    testAnalyzer classEnvironment' objectEnvironment sourceCode result =
      applyParameters classEnvironment' objectEnvironment (semanticCheck (parseExpression sourceCode)) `shouldBe` result
    testSubtype classEnvironment' possibleSubType parentType result =
      runReader ((possibleSubType <== parentType) :: Reader ClassEnvironment Bool) classEnvironment' `shouldBe` result
    testUpperBound classEnvironment' possibleSubType parentType result =
      runReader ((possibleSubType \/ parentType) :: Reader ClassEnvironment T.Type) classEnvironment' `shouldBe` result

applyParameters :: ClassEnvironment -> ObjectEnvironment -> SemanticAnalyzer a -> (a, [SemanticError])
applyParameters classEnvironment' objectEnvironment semanticAnalyzer =
  evalState (runWriterT (runReaderT semanticAnalyzer classEnvironment')) objectEnvironment
