{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module SemanticAnalyzer.SemanticCheckSpec
  ( main
  , spec
  ) where

import Test.Hspec (Spec, describe, hspec, it, shouldBe)

import Control.Monad.RWS.Lazy (evalRWS)
import Parser.ParserUtil (parse)
import SemanticAnalyzer.ClassEnvironments
import SemanticAnalyzer.SemanticAnalyzerRunner
import SemanticAnalyzer.SemanticCheck (semanticCheck)
import SemanticAnalyzer.TypeCheckError
import SemanticAnalyzer.TypedAST
       (ClassT(ClassT), ExpressionT(..), FeatureT(..), FormalT(..),
        LetBindingT(..), ProgramT(ProgramT))
import Util

main :: IO ()
main = hspec spec

testDirectory :: FilePath
testDirectory = "test/SemanticAnalyzer/Files/"

fooTree :: ClassT
fooTree =
  ClassT
    "Foo"
    "Object"
    [ MethodT "call8" [] "Int" (IntegerExprT 8)
    , MethodT
        "sum"
        [FormalT "a" "Int", FormalT "b" "Int"]
        "Int"
        (PlusExprT (IdentifierExprT "a" "Int") (IdentifierExprT "b" "Int"))
    ]

multipleErrorsTree :: ClassT
multipleErrorsTree =
  ClassT
    "MultipleErrors"
    "Object"
    [ AttributeT "eight" "Int" (Just $ StringExprT "8")
    , MethodT "add8" [FormalT "b" "Int"] "Int" (PlusExprT (StringExprT "8") (IdentifierExprT "b" "Int"))
    ]

programTree :: ProgramT
programTree = ProgramT [fooTree, ClassT "Bar" "Foo" [], ClassT "Quux" "Object" []]

spec :: Spec
spec =
  describe "Semantic Analysis" $ do
    describe "program" $
      it "should parse a program with multiple classes" $ do
        fileContents <- readFile $ testDirectory ++ "Program/Program.cl"
        testProgramAnalyzer classEnvironmentMock fileContents (programTree, [])
    describe "class" $ do
      it "should parse a class with no features" $
        testProgramAnalyzer classEnvironmentMock "class Foo {}" (ClassT "Foo" "Object" [], [])
      it "should parse a class with some features" $ do
        fileContents <- readFile $ testDirectory ++ "Class/Foo.cl"
        testProgramAnalyzer classEnvironmentMock fileContents (fooTree, [])
      it "should list all the errors for each class" $ do
        fileContents <- readFile $ testDirectory ++ "Class/MultipleErrors.cl"
        testProgramAnalyzer
          classErrorMock
          fileContents
          (multipleErrorsTree, [WrongSubtypeAttribute "eight" "String" "Int", NonIntArgumentsPlus "String" "Int"])
    describe "features" $ do
      describe "attributes" $ do
        describe "no initial expression" $ do
          it "should parse an attribute" $
            testAnalyzer "Foo" classEnvironmentMock [] "x : Int" (AttributeT "x" "Int" Nothing, [])
          it "should throw an error if an attribute is undefined" $
            testAnalyzer
              "Foo"
              classEnvironmentMock
              []
              "x : Undefined"
              (AttributeT "x" "Undefined" Nothing, [AttributeUndefinedDeclareType "x" "Undefined"])
        describe "has initial expression" $ do
          it "should not throw an error if initial expression type is undefined" $
            testAnalyzer
              "Foo"
              classEnvironmentMock
              ["x" =: "Undefined"]
              "y : Int <- x"
              (AttributeT "y" "Int" (Just (IdentifierExprT "x" "Undefined")), [])
          it "should throw an error if inferred expression type is not a subtype of declared type" $
            testAnalyzer
              "Foo"
              classEnvironmentMock
              []
              "y : String <- 5"
              (AttributeT "y" "String" (Just (IntegerExprT 5)), [WrongSubtypeAttribute "y" "Int" "String"])
      describe "methods" $ do
        it "should parse a function with no parameters" $
          testAnalyzer "Foo" classEnvironmentMock [] "call8 () :Int {8}" (MethodT "call8" [] "Int" (IntegerExprT 8), [])
        it "should throw an error if a parameter type is undefined" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "foo (x : Undefined) :Int {8}"
            (MethodT "foo" [FormalT "x" "Undefined"] "Int" (IntegerExprT 8), [UndefinedParameterType "x" "Undefined"])
        it "should throw an error if return type is not defined" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "foo() : Undefined {8}"
            (MethodT "foo" [] "Undefined" (IntegerExprT 8), [UndefinedReturnType "foo" "Undefined"])
        it "should not throw if inferrred expression type is undefined" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            ["x" =: "Undefined"]
            "callMessage() : String {x}"
            (MethodT "callMessage" [] "String" (IdentifierExprT "x" "Undefined"), [])
        it "should throw an error if the inferred expression is not a subtype of the return type" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "incorrectSubtype() : String {8}"
            ( MethodT "incorrectSubtype" [] "String" (IntegerExprT 8)
            , [WrongSubtypeMethod "incorrectSubtype" "Int" "String"])
        it "should parse correctly if a parameter is used in an expression" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "add8(x : Int) : Int { x + 8}"
            (MethodT "add8" [FormalT "x" "Int"] "Int" (PlusExprT (IdentifierExprT "x" "Int") (IntegerExprT 8)), [])
    describe "expression" $ do
      describe "binary arithmetic" $ do
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
        it "should annotate a new expression for a regular type" $
          testAnalyzer "Foo" classEnvironmentMock [] "new Foo" (NewExprT "Foo" "Foo", [])
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
            it "should throw an error if the declared type of a variable is not defined" $
              testAnalyzer'
                classEnvironmentMock
                []
                "let x : Undefined <- 4 in x"
                ( LetExprT $ LetBindingT "x" "Undefined" (Just $ IntegerExprT 4) (IdentifierExprT "x" "Undefined")
                , [LetUndefinedDeclareType "x" "Undefined"])
            it
              "should throw an error if the expression is using the the initialized identifier and that identifier is not called yet" $
              testAnalyzer'
                classEnvironmentMock
                []
                "let x : Int <- x in x"
                ( LetExprT $ LetBindingT "x" "Int" (Just $ IdentifierExprT "x" "Object") (IdentifierExprT "x" "Int")
                , [UndeclaredIdentifier "x", WrongSubtypeLet "x" "Object" "Int"])
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
              , [WrongSubtypeLet "x" "String" "Int"])
  --        it "should be able to use SELF_TYPE" $
  --          testAnalyzer
  --            "Foo"
  --            classEnvironmentMock
  --            []
  --            "let x: SELF_TYPE in x.call8() + 5"
  --            ( LetExprT $
  --              LetBindingT
  --                "x"
  --                "SELF_TYPE"
  --                Nothing
  --                (PlusExprT (MethodDispatchT (IdentifierExprT "x" "Int") "call8" [] "Int") (IntegerExprT 5))
  --            , [])
      describe "method dispatch" $ do
        it "should throw an error if a method could not be found for a class" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "foo()"
            (MethodDispatchT SelfVarExprT "foo" [] "Object", [UndefinedMethod "foo"])
        it "should throw an error if the caller expression returns an undefined class" $
          testAnalyzer
            "Foo"
            []
            ["baz" =: "Baz"]
            "baz.foo()"
            (MethodDispatchT (IdentifierExprT "baz" "Baz") "foo" [] "Object", [DispatchUndefinedClass "Baz"])
        it "should dispatch correctly when the caller expression type is different from the current class" $
          testAnalyzer
            "Quux"
            classEnvironmentMock
            ["x" =: "Foo"]
            "x.call8()"
            (MethodDispatchT (IdentifierExprT "x" "Foo") "call8" [] "Int", [])
        it "should parse if it can call a valid method with no parameters from a valid class" $
          testAnalyzer "Foo" classEnvironmentMock [] "call8()" (MethodDispatchT SelfVarExprT "call8" [] "Int", [])
        it "should throw an error if the number of parameters do not match" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "call8(8)"
            (MethodDispatchT SelfVarExprT "call8" [IntegerExprT 8] "Int", [WrongNumberParameters "call8"])
        it "should throw an error if a parameter is not a subtype of it's argument" $
          testAnalyzer
            "Foo"
            classEnvironmentMock
            []
            "sum(\"string\", 2)"
            ( MethodDispatchT SelfVarExprT "sum" [StringExprT "string", IntegerExprT 2] "Int"
            , [WrongParameterType "sum" "a" "Int" "String"])
--        it "should have the return type of a method that returns SELF_TYPE be the class it represents" $
--          testAnalyzer
--            "Foo"
--            ["Foo" =: ClassRecord "Foo" ObjectClass ["callSelf" =: MethodRecord "callSelf" [] "SELF_TYPE"] []]
--            []
--            "callSelf()"
--            (MethodDispatchT SelfVarExprT "callSelf" [] "Foo", [])
      describe "static method dispatch" $ do
        it "should throw an error when a class refers to undefined method" $
          testAnalyzer
            "Bar"
            classEnvironmentMock
            ["x" =: "Bar"]
            "x@X.call8()"
            (StaticMethodDispatchT (IdentifierExprT "x" "Bar") "X" "call8" [] "Object", [UndefinedStaticDispatch "X"])
        it "should throw an error when a class refers to method" $
          testAnalyzer
            "Bar"
            classEnvironmentMock
            ["x" =: "Bar"]
            "x@Quux.call8()"
            ( StaticMethodDispatchT (IdentifierExprT "x" "Bar") "Quux" "call8" [] "Object"
            , [WrongSubtypeStaticDispatch "Bar" "Quux"])
        it "should parse static dispatch correctly" $
          testAnalyzer
            "Quux"
            classEnvironmentMock
            ["x" =: "Bar"]
            "x@Foo.call8()"
            (StaticMethodDispatchT (IdentifierExprT "x" "Bar") "Foo" "call8" [] "Int", [])
  where
    testAnalyzer currentClassName classEnvironment objectEnvironment sourceCode result =
      evalRWS (semanticCheck (parse sourceCode)) (currentClassName, classEnvironment) objectEnvironment `shouldBe`
      result
    testAnalyzer' = testAnalyzer ""
    testProgramAnalyzer classEnvironment sourceCode result =
      runProgramAnalyzer classEnvironment (semanticCheck (parse sourceCode)) `shouldBe` result
