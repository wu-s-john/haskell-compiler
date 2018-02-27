{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassCheckerSpec
  ( main
  , spec
  ) where

import qualified Data.Map as M
import Parser.Parser (programParser)
import Parser.ParserUtil (stringToAST)
import SemanticAnalyzer.ClassChecker
import Test.Hspec (Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "classChecker" $ do
    describe "createTypeMap" $ do
      it "should parse one class of a program into a Map" $
        toClassInheritanceMap "class Foo {};" `shouldBe` (OK $ M.fromList [("Foo", "Object")])
      it "should parse multiple classes of a program into a Map" $
        toClassInheritanceMap "class A {}; class B inherits A {}; class C inherits B {}; class D inherits A {};" `shouldBe`
        (OK $ M.fromList [("A", "Object"), ("B", "A"), ("C", "B"), ("D", "A")])
      it "should have an error invoked if a single class occurs multiple times" $
        toClassInheritanceMap "class A {}; class A inherits B {}; " `shouldBe`
        Error [PreviouslyDefined "A"] (M.fromList [("A", "Object")])
      it "should have an error invoked if multiple classes occurs multiple times" $
        toClassInheritanceMap "class A inherits B {}; class A {}; class Foo {}; class Foo {}; class A {}; class A {};" `shouldBe`
        Error
          [PreviouslyDefined "A", PreviouslyDefined "A", PreviouslyDefined "Foo", PreviouslyDefined "A"]
          (M.fromList [("A", "B"), ("Foo", "Object")])
    describe "checkPrimitiveInheritance" $ do
      it "should determine if a class inherited a primitive type (bool)" $
        checkPrimitiveInheritance "Bool" `shouldBe` True
      it "should determine if a class inherited a SELF_TYPE" $ checkPrimitiveInheritance "SELF_TYPE" `shouldBe` True
      it "should determine if a class inherited a custom created type" $
        checkPrimitiveInheritance "Foo" `shouldBe` False
    describe "checkIllegalInheritance" $ do
      it "should not have a class inherit from a primitive type" $
        checkIllegalInheritance (M.fromList [("Foo", "Bool")]) `shouldBe` [PrimitiveInheritance "Foo" "Bool"]
      it "should not have a class inherit from an undefined class" $
        checkIllegalInheritance (M.fromList [("Foo", "Bar")]) `shouldBe` [UndefinedInheritance "Foo" "Bar"]
      it "should not have a class inherit from multiple classes" $
        checkIllegalInheritance (M.fromList [("A", "SELF_TYPE"), ("B", "A"), ("C", "Baz")]) `shouldBe`
        [PrimitiveInheritance "A" "SELF_TYPE", UndefinedInheritance "C" "Baz"]

toClassInheritanceMap :: String -> ClassAnalysisResult
toClassInheritanceMap = createTypeMap . stringToAST programParser
