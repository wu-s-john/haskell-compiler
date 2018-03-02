{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassSpec
  ( main
  , spec
  ) where

import qualified Parser.AST as AST
import SemanticAnalyzer.Class
       (AttributeRecord, MethodRecord(..), extractAttributeRecord,
        extractMethodRecord, AttributeRecord(..))
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "class" $ do
    describe "method" $ do
      it "should not parse an attribute" $ extractMethodRecord (AST.Attribute "foo" "Bar" Nothing) `shouldBe` Nothing
      it "should parse a method with no parameters" $
        extractMethodRecord (AST.Method "foo" [] "Bar" AST.TrueExpr) `shouldBe` (Just $ MethodRecord "foo" [] "Bar")
      it "should parse a method with parameters" $
        extractMethodRecord (AST.Method "foo" [AST.Formal "x" "X", AST.Formal "y" "Y"] "Foo" AST.TrueExpr) `shouldBe`
        (Just $ MethodRecord "foo" [AST.Formal "x" "X", AST.Formal "y" "Y"] "Foo")
    describe "attribute" $ do
      it "should not parse a method" $ extractAttributeRecord (AST.Method "foo" [] "Bar" AST.TrueExpr) `shouldBe` Nothing
      it "should parse an attribute" $
        extractAttributeRecord (AST.Attribute "foo" "Bar" Nothing) `shouldBe` (Just $ AttributeRecord "foo" "Bar")
