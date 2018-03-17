{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.SemanticCheckUtilSpec
  ( main
  , spec
  ) where

import qualified Data.Map as M

import Test.Hspec (Spec, describe, hspec, it,shouldBe)
import Control.Monad.State (get)

import SemanticAnalyzer.SemanticCheckUtil ((/>), (<==), (\/))
import SemanticAnalyzer.Util

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "SemanticCheckUtil" $ do
    describe "/>" $
          it "should set a new variable and is a pplied only to an inner scope" $
          fst $
          applyParameters "" [] [] $ do
            _ <- ("x", "Int") /> (get >>= (\objectEnvironment -> return $ "x" `M.member` objectEnvironment `shouldBe` True))
            objectEnvironment <- get
            return $ "x" `M.member` objectEnvironment `shouldBe` False
    describe "isSubtype" $ -- todo separate the cases for (String and Int) with IO
     do
      it "a basic class should be a subtype of object" $ testSubtype' classEnvironmentMock "String" "Object" True
      it "a basic class should not be a subtype of any constructed type" $
        testSubtype' classEnvironmentMock "String" "Foo" False
      it "a basic class should be a subtype of a basic class if they are the same" $
        testSubtype' classEnvironmentMock "String" "String" True
      it "a basic class should not be a subtype of a basic class if they are not the same" $
        testSubtype' classEnvironmentMock "String" "Int" False
      it "a class record should be a subtype of a basic class only if it inherits the class" $
        testSubtype' classEnvironmentWithInheritedBasicClass "Baz" "Int" True
      it "a class record should not be a subtype of a basic class if it does not inherit the class" $
        testSubtype' classEnvironmentMock "Baz" "Int" False
      it "should return true if two types are subtypes of each other" $
        testSubtype' classEnvironmentMock "Bar" "Foo" True
      it "should return false if two types are not subtypes of each other" $
        testSubtype' classEnvironmentMock "Foo" "Bar" False
      it "should return false if the possible subtype is undefined" $ testSubtype' classEnvironmentMock "X" "Foo" False
      it "should return false if the parent is undefined" $ testSubtype' classEnvironmentMock "Foo" "X" False
      it "should return true if the parent subtype is Object" $ testSubtype' classEnvironmentMock "Foo" "Object" True
    describe "lub (lowest upper bound)" $ do
      it "should return object if two types do not share the same ancestors" $
        testUpperBound' classEnvironmentMock "Foo" "X" "Object"
      it "should compute the lub of two types that share the same ancestors" $
        testUpperBound' classEnvironmentMock "Foo" "Foo" "Foo"
      it "should have the lub of a basic class and an object that doesn't inherit from a basic class be an Object" $
        testUpperBound' classEnvironmentMock "Foo" "Int" "Object"
      it "should have the lub of a basic class and an object that inherits from a basic class be the basic class" $
        testUpperBound' classEnvironmentWithInheritedBasicClass "Baz" "Int" "Int"
  where
    testSubtype currentClassName classEnvironment possibleSubType parentType result =
      fst (applyParameters currentClassName classEnvironment [] (possibleSubType <== parentType)) `shouldBe` result
    testSubtype' = testSubtype ""
    testUpperBound currentClassName classEnvironment possibleSubType parentType result =
      fst (applyParameters currentClassName classEnvironment [] (possibleSubType \/ parentType)) `shouldBe` result
    testUpperBound' = testUpperBound ""
