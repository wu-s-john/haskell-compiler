{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Parser.AST
import Parser.ParserUtil
import Test.Hspec
       (Expectation, Spec, anyException, describe, hspec, it, shouldBe,
        shouldThrow)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parser" $
  describe "unit-tests" $ do
    describe "expressions" $ do
      it "should parse <-" $ "foo <- 2" |-> AssignmentExpr "foo" (IntegerExpr 2)
      describe "dispatch" $ do
        it "should parse a method  dispatch with no calls and no tied expression" $
          "foo()" |-> MethodDispatch SelfVarExpr "foo" []
        it "should parse a method  dispatch with one parameter and no tied expression" $
          "foo(1)" |-> MethodDispatch SelfVarExpr "foo" [IntegerExpr 1]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo(1, bar)" |-> MethodDispatch SelfVarExpr "foo" [IntegerExpr 1, IdentifierExpr "bar"]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo.call(1, bar)" |-> MethodDispatch (IdentifierExpr "foo") "call" [IntegerExpr 1, IdentifierExpr "bar"]
        it "should call parse a method with multiple parameters and a call to static methods" $
          "foo@Foo.call(1, bar)" |->
          StaticMethodDispatch (IdentifierExpr "foo") "Foo" "call" [IntegerExpr 1, IdentifierExpr "bar"]
      it "should parse a while loop" $
        "while true loop foo() pool" |-> LoopExpr TrueExpr (MethodDispatch SelfVarExpr "foo" [])
      it "should parse conditional expressions" $
        "if 1 then \"foo\" else \"bar\" fi" |-> CondExpr (IntegerExpr 1) (StringExpr "foo") (StringExpr "bar")
      it "should parse a block" $ "{ 6; foo; }" |-> BlockExpr [IntegerExpr 6, IdentifierExpr "foo"]
      describe "let" $ do
        it "should parse a let expression with no expression" $
          "let foo : Bar in foo" |-> LetExpr (LetBinding "foo" "Bar" Nothing (IdentifierExpr "foo"))
        it "should parse a let expression that has an initialized expression expression" $
          "let foo : Bar <- baz in foo" |->
          LetExpr (LetBinding "foo" "Bar" (Just (IdentifierExpr "baz")) (IdentifierExpr "foo"))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, z:Int in x" |->
          LetExpr (LetDeclaration "x" "Int" (Just (IntegerExpr 5)) (LetBinding "z" "Int" Nothing (IdentifierExpr "x")))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, y:String, z:Int in x" |->
          LetExpr
            (LetDeclaration
               "x"
               "Int"
               (Just (IntegerExpr 5))
               (LetDeclaration "y" "String" Nothing (LetBinding "z" "Int" Nothing (IdentifierExpr "x"))))
        it "should create an error if let has no identifier bindings" $ (@+@) "let in x"
      describe "typecases" $ do
        it "should parse a type case" $
          "case foo of x: Int => 3; y: String => \"foo\"; esac " |->
          TypeCaseExpr
            (IdentifierExpr "foo")
            [CaseBranch "x" "Int" (IntegerExpr 3), CaseBranch "y" "String" (StringExpr "foo")]
        it "should create an error if there are no case branches" $(@+@)  "case foo of x: esac"
      it "should parse new" $ "new Foo" |-> NewExpr "Foo"
      it "should parse isvoid" $ "isvoid foo" |-> IsvoidExpr (IdentifierExpr "foo")
      it "should parse a binary expression" $
        "1 + 2 * 5" |-> PlusExpr (IntegerExpr 1) (TimesExpr (IntegerExpr 2) (IntegerExpr 5))
      it "should parse division" $ "6 / 2" |-> DivideExpr (IntegerExpr 6) (IntegerExpr 2)
      it "should parse ~" $ "~ foo" |-> NegExpr (IdentifierExpr "foo")
      it "should parse <" $ "1 < 2" |-> LessThanExpr (IntegerExpr 1) (IntegerExpr 2)
      it "should parse <=" $ "1 <= 2" |-> LessThanOrEqualExpr (IntegerExpr 1) (IntegerExpr 2)
      it "should parse =" $ "2 = 2" |-> EqualExpr (IntegerExpr 2) (IntegerExpr 2)
      it "should parse not" $ "not foo" |-> NotExpr (IdentifierExpr "foo")
      it "should parse ()" $ "(foo)" |-> IdentifierExpr "foo"
      it "should parse an identifier expression" $ "foo" |-> IdentifierExpr "foo"
      it "should parse a string" $ "\"foo\"" |-> StringExpr "foo"
      it "should parse a true" $ "true" |-> TrueExpr
      it "should parse a false" $ "false" |-> FalseExpr
    describe "features" $ do
      it "should parse a feature attribute" $ "foo : Foo" |-> Attribute "foo" "Foo" Nothing
      it "should parse a feature attribute assigned to an expression" $
        "foo : Foo <- bar" |-> Attribute "foo" "Foo" (Just (IdentifierExpr "bar"))
      describe "methods" $ do
        it "should parse a feature method with no parameters" $
          "call() : Foo {\"string\"}" |-> Method "call" [] "Foo" (StringExpr "string")
        it "should parse a feature method with one parameter" $
          "identity(x: Int) : Int { x }" |-> Method "identity" [Formal "x" "Int"] "Int" (IdentifierExpr "x")
        it "should parse a feature method with multiple parameters" $
          "sum(x : Int, y : Int) : Int { x + y }" |->
          Method "sum" [Formal "x" "Int", Formal "y" "Int"] "Int" (PlusExpr (IdentifierExpr "x") (IdentifierExpr "y"))
      it "should parse a multiple features" $
        "foo : Foo <- bar; \n    x : Y;\n" |->
        [Attribute "foo" "Foo" (Just (IdentifierExpr "bar")), Attribute "x" "Y" Nothing]
    describe "class" $ do
      it "should parse an orphaned class" $ "class Foo {\n\n}\n" |-> Class "Foo" "Object" []
      it "should parse an inherited class" $ "class Foo inherits Bar {\n\n}\n" |-> Class "Foo" "Bar" []
    describe "program" $ do
      it "should parse a single program" $
        "class Hello {\n    foo : Int;\n    bar : String;\n};\n" |->
        Program [Class "Hello" "Object" [Attribute "foo" "Int" Nothing, Attribute "bar" "String" Nothing]]
      it "should parse a single program" $
        "class Foo {}; class Bar {};\n" |-> Program [Class "Foo" "Object" [], Class "Bar" "Object" []]

(|->) ::
     Show a
  => Eq a =>
       Parsable a =>
         String -> a -> Expectation
code |-> expectedResult = parse code `shouldBe` expectedResult

(@+@) :: String -> Expectation
(@+@) code = evaluate (parse code :: Expression) `shouldThrow` anyException
