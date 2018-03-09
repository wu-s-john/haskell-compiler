{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Parser.AST
import Parser.Parser
       (classParser, expressionParser, featureParser, featuresParser,
        programParser)
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
      it "should parse <-" $ "foo <- 2" `testExpression` AssignmentExpr "foo" (IntegerExpr 2)
      describe "dispatch" $ do
        it "should parse a method  dispatch with no calls and no tied expression" $
          "foo()" `testExpression` MethodDispatch SelfVarExpr "foo" []
        it "should parse a method  dispatch with one parameter and no tied expression" $
          "foo(1)" `testExpression` MethodDispatch SelfVarExpr "foo" [IntegerExpr 1]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo(1, bar)" `testExpression` MethodDispatch SelfVarExpr "foo" [IntegerExpr 1, IdentifierExpr "bar"]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo.call(1, bar)" `testExpression`
          MethodDispatch (IdentifierExpr "foo") "call" [IntegerExpr 1, IdentifierExpr "bar"]
        it "should call parse a method with multiple parameters and a call to static methods" $
          "foo@Foo.call(1, bar)" `testExpression`
          StaticMethodDispatch (IdentifierExpr "foo") "Foo" "call" [IntegerExpr 1, IdentifierExpr "bar"]
      it "should parse a while loop" $
        "while true loop foo() pool" `testExpression` LoopExpr TrueExpr (MethodDispatch SelfVarExpr "foo" [])
      it "should parse conditional expressions" $
        "if 1 then \"foo\" else \"bar\" fi" `testExpression`
        CondExpr (IntegerExpr 1) (StringExpr "foo") (StringExpr "bar")
      it "should parse a block" $ "{ 6; foo; }" `testExpression` BlockExpr [IntegerExpr 6, IdentifierExpr "foo"]
      describe "let" $ do
        it "should parse a let expression with no expression" $
          "let foo : Bar in foo" `testExpression` LetExpr (LetBinding "foo" "Bar" Nothing (IdentifierExpr "foo"))
        it "should parse a let expression that has an initialized expression expression" $
          "let foo : Bar <- baz in foo" `testExpression`
          LetExpr (LetBinding "foo" "Bar" (Just (IdentifierExpr "baz")) (IdentifierExpr "foo"))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, z:Int in x" `testExpression`
          LetExpr (LetDeclaration "x" "Int" (Just (IntegerExpr 5)) (LetBinding "z" "Int" Nothing (IdentifierExpr "x")))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, y:String, z:Int in x" `testExpression`
          LetExpr
            (LetDeclaration
               "x"
               "Int"
               (Just (IntegerExpr 5))
               (LetDeclaration "y" "String" Nothing (LetBinding "z" "Int" Nothing (IdentifierExpr "x"))))
        it "should create an error if let has no identifier bindings" $
          evaluate (parseExpression "let in x") `shouldThrow` anyException
      describe "typecases" $ do
        it "should parse a type case" $
          "case foo of x: Int => 3; y: String => \"foo\"; esac " `testExpression`
          TypeCaseExpr
            (IdentifierExpr "foo")
            [CaseBranch "x" "Int" (IntegerExpr 3), CaseBranch "y" "String" (StringExpr "foo")]
        it "should create an error if there are no case branches" $
          evaluate (parseExpression "case foo of x: esac") `shouldThrow` anyException
      it "should parse new" $ "new Foo" `testExpression` NewExpr "Foo"
      it "should parse isvoid" $ "isvoid foo" `testExpression` IsvoidExpr (IdentifierExpr "foo")
      it "should parse a binary expression" $
        "1 + 2 * 5" `testExpression` PlusExpr (IntegerExpr 1) (TimesExpr (IntegerExpr 2) (IntegerExpr 5))
      it "should parse division" $ "6 / 2" `testExpression` DivideExpr (IntegerExpr 6) (IntegerExpr 2)
      it "should parse ~" $ "~ foo" `testExpression` NegExpr (IdentifierExpr "foo")
      it "should parse <" $ "1 < 2" `testExpression` LessThanExpr (IntegerExpr 1) (IntegerExpr 2)
      it "should parse <=" $ "1 <= 2" `testExpression` LessThanOrEqualExpr (IntegerExpr 1) (IntegerExpr 2)
      it "should parse =" $ "2 = 2" `testExpression` EqualExpr (IntegerExpr 2) (IntegerExpr 2)
      it "should parse not" $ "not foo" `testExpression` NotExpr (IdentifierExpr "foo")
      it "should parse ()" $ "(foo)" `testExpression` IdentifierExpr "foo"
      it "should parse an identifier expression" $ "foo" `testExpression` IdentifierExpr "foo"
      it "should parse a string" $ "\"foo\"" `testExpression` StringExpr "foo"
      it "should parse a true" $ "true" `testExpression` TrueExpr
      it "should parse a false" $ "false" `testExpression` FalseExpr
    describe "features" $ do
      it "should parse a feature attribute" $ "foo : Foo" `testFeature` Attribute "foo" "Foo" Nothing
      it "should parse a feature attribute assigned to an expression" $
        "foo : Foo <- bar" `testFeature` Attribute "foo" "Foo" (Just (IdentifierExpr "bar"))
      describe "methods" $ do
        it "should parse a feature method with no parameters" $
          "call() : Foo {\"string\"}" `testFeature` Method "call" [] "Foo" (StringExpr "string")
        it "should parse a feature method with one parameter" $
          "identity(x: Int) : Int { x }" `testFeature` Method "identity" [Formal "x" "Int"] "Int" (IdentifierExpr "x")
        it "should parse a feature method with multiple parameters" $
          "sum(x : Int, y : Int) : Int { x + y }" `testFeature`
          Method "sum" [Formal "x" "Int", Formal "y" "Int"] "Int" (PlusExpr (IdentifierExpr "x") (IdentifierExpr "y"))
      it "should parse a multiple features" $
        testParser
          (stringToAST featuresParser)
          "foo : Foo <- bar; \n    x : Y;\n"
          [Attribute "foo" "Foo" (Just (IdentifierExpr "bar")), Attribute "x" "Y" Nothing]
    describe "class" $ do
      it "should parse an orphaned class" $ testClass "class Foo {\n\n}\n" (Class "Foo" "Object" [])
      it "should parse an inherited class" $ testClass "class Foo inherits Bar {\n\n}\n" (Class "Foo" "Bar" [])
    describe "program" $ do
      it "should parse a single program" $
        testProgram
          "class Hello {\n    foo : Int;\n    bar : String;\n};\n"
          (Program [Class "Hello" "Object" [Attribute "foo" "Int" Nothing, Attribute "bar" "String" Nothing]])
      it "should parse a single program" $
        testProgram "class Foo {}; class Bar {};\n" (Program [Class "Foo" "Object" [], Class "Bar" "Object" []])

testParser ::
     Show a
  => Eq a =>
       (String -> a) -> String -> a -> Expectation
testParser parser code expectedResult = parser code `shouldBe` expectedResult

testClass :: String -> Class -> Expectation
testClass = testParser (stringToAST classParser)

testFeature :: String -> Feature -> Expectation
testFeature = testParser (stringToAST featureParser)

testExpression :: String -> Expression -> Expectation
testExpression = testParser (stringToAST expressionParser)

testProgram :: String -> Program -> Expectation
testProgram = testParser parseProgram
