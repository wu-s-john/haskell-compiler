{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Control.Exception (evaluate)
import Parser.AST
import Parser.Parser
       (classParser, expressionParser, featureParser, featuresParser)
import Parser.ParserUtil
import Parser.TerminalNode
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
      it "should parse <-" $ "foo <- 2" `testExpression` AssignmentExpr (Identifier "foo") (IntegerExpr 2)
      describe "dispatch" $ do
        it "should parse a method  dispatch with no calls and no tied expression" $
          "foo()" `testExpression` MethodDispatch SelfVarExpr (Identifier "foo") []
        it "should parse a method  dispatch with one parameter and no tied expression" $
          "foo(1)" `testExpression` MethodDispatch SelfVarExpr (Identifier "foo") [IntegerExpr 1]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo(1, bar)" `testExpression`
          MethodDispatch SelfVarExpr (Identifier "foo") [IntegerExpr 1, IdentifierExpr "bar"]
        it "should parse a method dispatch with multiple parameters and no tied expression" $
          "foo.call(1, bar)" `testExpression`
          MethodDispatch (IdentifierExpr "foo") (Identifier "call") [IntegerExpr 1, IdentifierExpr "bar"]
        it "should call parse a method with multiple parameters and a call to static methods" $
          "foo@Foo.call(1, bar)" `testExpression`
          StaticMethodDispatch
            (IdentifierExpr "foo")
            (Type "Foo")
            (Identifier "call")
            [IntegerExpr 1, IdentifierExpr "bar"]
      it "should parse a while loop" $
        "while true loop foo() pool" `testExpression`
        LoopExpr TrueExpr (MethodDispatch SelfVarExpr (Identifier "foo") [])
      it "should parse conditional expressions" $
        "if 1 then \"foo\" else \"bar\" fi" `testExpression`
        CondExpr (IntegerExpr 1) (StringExpr "foo") (StringExpr "bar")
      it "should parse a block" $ "{ 6; foo; }" `testExpression` BlockExpr [IntegerExpr 6, IdentifierExpr "foo"]
      describe "let" $ do
        it "should parse a let expression with no expression" $
          "let foo : Bar in foo" `testExpression`
          LetExpr (LetBinding (Identifier "foo") (Type "Bar") Nothing (IdentifierExpr "foo"))
        it "should parse a let expression that has an initialized expression expression" $
          "let foo : Bar <- baz in foo" `testExpression`
          LetExpr (LetBinding (Identifier "foo") (Type "Bar") (Just (IdentifierExpr "baz")) (IdentifierExpr "foo"))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, z:Int in x" `testExpression`
          LetExpr
            (LetDeclaration
               (Identifier "x")
               (Type "Int")
               (Just (IntegerExpr 5))
               (LetBinding (Identifier "z") (Type "Int") Nothing (IdentifierExpr "x")))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, y:String, z:Int in x" `testExpression`
          LetExpr
            (LetDeclaration
               (Identifier "x")
               (Type "Int")
               (Just (IntegerExpr 5))
               (LetDeclaration
                  (Identifier "y")
                  (Type "String")
                  Nothing
                  (LetBinding (Identifier "z") (Type "Int") Nothing (IdentifierExpr "x"))))
        it "should create an error if let has no identifier bindings" $
          evaluate (stringToAST expressionParser "let in x") `shouldThrow` anyException
      describe "typecases" $ do
        it "should parse a type case" $
          "case foo of x: Int => 3; y: String => \"foo\"; esac " `testExpression`
          TypeCaseExpr
            (IdentifierExpr "foo")
            [ CaseBranch (Identifier "x") (Type "Int") (IntegerExpr 3)
            , CaseBranch (Identifier "y") (Type "String") (StringExpr "foo")
            ]
        it "should create an error if there are no case branches" $
          evaluate (stringToAST expressionParser "case foo of x: esac") `shouldThrow` anyException
      it "should parse new" $ "new Foo" `testExpression` NewExpr (Type "Foo")
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
      it "should parse a feature attribute" $
        "foo : Foo" `testFeature` createAttribute "foo" "Foo" Nothing
      it "should parse a feature attribute assigned to an expression" $
        "foo : Foo <- bar" `testFeature` createAttribute "foo" "Foo" (Just (IdentifierExpr "bar"))
      describe "methods" $ do
        it "should parse a feature method with no parameters" $
          "call() : Foo {\"string\"}" `testFeature` createMethod "call" "Foo" [] (StringExpr "string")
        it "should parse a feature method with one parameter" $
          "identity(x: Int) : Int { x }" `testFeature`
          createMethod "identity" "Int" [createFormal "x" "Int"] (IdentifierExpr "x")
        it "should parse a feature method with multiple parameters" $
          "sum(x : Int, y : Int) : Int { x + y }" `testFeature`
          createMethod
            "sum"
            "Int"
            [createFormal "x" "Int", createFormal "y" "Int"]
            (PlusExpr (IdentifierExpr "x") (IdentifierExpr "y"))
      it "should parse a multiple features" $
        testParser
          (stringToAST featuresParser)
          "foo : Foo <- bar; \n    x : Y;\n"
          [ createAttribute "foo" "Foo" (Just (IdentifierExpr "bar"))
          , createAttribute "x" "Y" Nothing
          ]
    describe "class" $ do
      it "should parse an orphaned class" $ testClass "class Foo {\n\n}\n" (OrphanedClass (Type "Foo") [])
      it "should parse an inherited class" $
        testClass "class Foo inherits Bar {\n\n}\n" (InheritedClass (Type "Foo") (Type "Bar") [])

createMethod :: String -> String -> [Formal] -> Expression -> Feature
createMethod methodName typeName parameters = Method (Identifier methodName) parameters (Type typeName)

createAttribute :: String -> String -> Maybe Expression -> Feature
createAttribute attrName typeName = Attribute (Identifier attrName) (Type typeName)

createFormal :: String -> String -> Formal
createFormal identifierName typeName = Formal (Identifier identifierName) (Type typeName)

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
