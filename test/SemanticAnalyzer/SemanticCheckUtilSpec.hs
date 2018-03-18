{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.SemanticCheckUtilSpec
  ( main
  , spec
  ) where

import qualified Data.Map as M

import Control.Monad.State (get)
import Test.Hspec (Spec, describe, hspec, it, shouldBe,Expectation)

import SemanticAnalyzer.Class (ClassRecord(..))
import SemanticAnalyzer.InitialClassEnvironment
import SemanticAnalyzer.SemanticCheckUtil ((/>), (<==), (\/))
import SemanticAnalyzer.Util
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.Type (Type)

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
    describe "isSubtype" $
     do
      it "any class should be a subtype of object" $ testSubtype' classEnvironmentMock "String" "Object" True
      it "should not have a primitive type be a subtype of any class (except for Object)" $
        testSubtype' classEnvironmentMock "String" "Foo" False
      it "should have a primitive type be a subtype of itself" $
        testSubtype' classEnvironmentMock "String" "String" True
      it "should not have a class be a subtype of another class it does not inherit the class" $
        testSubtype' classEnvironmentMock "Baz" "Quux" False
      it "should have a child class be a subtype of parent class" $ testSubtype' classEnvironmentMock "Bar" "Foo" True
      it "should not have a parent class be a subtype of it's child class" $
        testSubtype' classEnvironmentMock "Foo" "Bar" False
      it "should not have a class be a subtype of an undefined class" $
        testSubtype' classEnvironmentMock "X" "Foo" False
      it "should not have an undefined class be a subtype of a defined class" $
        testSubtype' classEnvironmentMock "Foo" "X" False
      it "should have any defined class be a subclass of an object" $
        testSubtype' classEnvironmentMock "Foo" "Object" True
      it "should allow a class be a subtype of IO" $
        testSubtype' (M.insert "ChildIO" (ClassRecord "ChildIO" ioRecord [] []) classEnvironmentMock) "ChildIO" "IO" True
      describe "IO" $ do
        it "should have IO be a subtype of itself" $ testSubtype' classEnvironmentMock "IO" "Object" True
        it "should have IO be a subtype of Object" $ testSubtype' classEnvironmentMock "IO" "IO" True
        it "should not have IO be a subtype of a basic object" $ testSubtype' classEnvironmentMock "IO" "Foo" False
    describe "lub (lowest upper bound)" $ do
      it "should compute the lub of two types do not share the same ancestors be object" $
        testUpperBound' classEnvironmentMock "Foo" "X" "Object"
      it "should compute the lub of two types that are the same type to be that type" $
        testUpperBound' classEnvironmentMock "Foo" "Foo" "Foo"
      it "should compute the lub of two types where one is a subtype of the other to be the supertype" $
        testUpperBound' classEnvironmentMock "Foo" "Bar" "Foo"
      it "should have the lub of a basic class and an object that doesn't inherit from a basic class be an Object" $
        testUpperBound' classEnvironmentMock "Foo" "Int" "Object"


testSubtype :: String -> ClassEnvironment -> Type -> Type -> Bool -> Expectation
testSubtype currentClass classEnvironment possibleSubType parentType result =
  fst (applyParameters currentClass classEnvironment [] (possibleSubType <== parentType)) `shouldBe` result

testSubtype' :: ClassEnvironment -> Type -> Type -> Bool -> Expectation
testSubtype' = testSubtype ""

testUpperBound :: String -> ClassEnvironment -> Type -> Type -> Type -> Expectation
testUpperBound currentClass classEnvironment possibleSubType parentType result =
  fst (applyParameters currentClass classEnvironment [] (possibleSubType \/ parentType)) `shouldBe` result

testUpperBound' :: ClassEnvironment -> Type -> Type -> Type -> Expectation
testUpperBound' = testUpperBound ""
