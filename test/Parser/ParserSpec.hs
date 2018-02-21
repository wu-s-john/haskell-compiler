{-# OPTIONS_GHC -Wall #-}

module Parser.ParserSpec
  ( main
  , spec
  ) where

import Parser.AST
import Parser.Parser
       (classParser, expressionParser, featureParser, featuresParser)
import Parser.ParserUtil
import Parser.TerminalNode
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "parser" $
  describe "unit-tests" $ do
    describe "expressions" $ do
      it "should parse a binary expression" $
        "1 + 2 * 5" `testExpression`
        BinaryOp PlusTerminal (IntegerExpr 1) (BinaryOp TimesTerminal (IntegerExpr 2) (IntegerExpr 5))
      it "should parse division" $ "6 / 2" `testExpression` BinaryOp DivideTerminal (IntegerExpr 6) (IntegerExpr 2)
      it "should parse an identifier expression" $ "foo" `testExpression` IdentifierExpr "foo"
      it "should parse a block" $ "{ 6; foo; }" `testExpression` BlockExpression [IntegerExpr 6, IdentifierExpr "foo"]
      it "should parse <" $ "1 < 2" `testExpression` BinaryOp LessThanTerminal (IntegerExpr 1) (IntegerExpr 2)
      it "should parse <=" $ "1 <= 2" `testExpression` BinaryOp LessThanOrEqualTerminal (IntegerExpr 1) (IntegerExpr 2)
      it "should parse =" $ "2 = 2" `testExpression` BinaryOp EqualTerminal (IntegerExpr 2) (IntegerExpr 2)
      it "should parse <-" $ "foo <- 2" `testExpression` AssignmentExpression (IdentifierExpr "foo") (IntegerExpr 2)
      it "should parse not" $ "not foo" `testExpression` UnaryOp NotTerminal (IdentifierExpr "foo")
      it "should parse isvoid" $ "isvoid foo" `testExpression` UnaryOp IsvoidTerminal (IdentifierExpr "foo")
      it "should parse ~" $ "~ foo" `testExpression` UnaryOp TildeTerminal (IdentifierExpr "foo")
      it "should parse new" $ "new Foo" `testExpression` NewExpression (Type "Foo")
      it "should parse a string" $ "\"foo\"" `testExpression` StringExpr "foo"
      describe "let" $ do
        it "should parse a let expression with no expression" $
          "let foo : Bar in foo" `testExpression`
          LetExpression (LetBinding (Identifier "foo") (Type "Bar") Nothing (IdentifierExpr "foo"))
        it "should parse a let expression that has an initialized expression expression" $
          "let foo : Bar <- baz in foo" `testExpression`
          LetExpression
            (LetBinding (Identifier "foo") (Type "Bar") (Just (IdentifierExpr "baz")) (IdentifierExpr "foo"))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, z:Int in x" `testExpression`
          LetExpression
            (LetDeclaration
               (Identifier "x")
               (Type "Int")
               (Just (IntegerExpr 5))
               (LetBinding (Identifier "z") (Type "Int") Nothing (IdentifierExpr "x")))
        it "should parse a let with multiple declaractions" $
          "let x:Int <- 5, y:String, z:Int in x" `testExpression`
          LetExpression
            (LetDeclaration
               (Identifier "x")
               (Type "Int")
               (Just (IntegerExpr 5))
               (LetDeclaration
                  (Identifier "y")
                  (Type "String")
                  Nothing
                  (LetBinding (Identifier "z") (Type "Int") Nothing (IdentifierExpr "x"))))
    describe "features" $ do
      it "should parse a feature" $ testFeature "foo : Foo" $ Attribute (Identifier "foo") (Type "Foo") Nothing
      it "should parse a feature assigned to an expression" $
        testFeature "foo : Foo <- bar" $ Attribute (Identifier "foo") (Type "Foo") (Just (IdentifierExpr "bar"))
      it "should parse a multiple features" $
        testParser
          (stringToAST featuresParser)
          "foo : Foo <- bar; \n    x : Y;\n"
          [ Attribute (Identifier "foo") (Type "Foo") (Just (IdentifierExpr "bar"))
          , Attribute (Identifier "x") (Type "Y") Nothing
          ]
    describe "class" $ do
      it "should parse an orphaned class" $ testClass "class Foo {\n\n}\n" (OrphanedClass (Type "Foo") [])
      it "should parse an inherited class" $
        testClass "class Foo inherits Bar {\n\n}\n" (InheritedClass (Type "Foo") (Type "Bar") [])

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
