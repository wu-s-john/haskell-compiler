{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassCheckerSpec
  ( main
  , spec
  ) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (State(..), evalState, execState, put)
import Control.Monad.Writer (execWriterT, runWriterT)
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.Parser (programParser)
import Parser.ParserUtil (stringToAST)
import Parser.TerminalNode (Type)
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
    describe "checkAcyclicErrors" $ do
      it "should have no errors if the graph is empty" $ checkAcyclicErrors M.empty `shouldBe` []
      it "should have no errors if there is a single object" $
        checkAcyclicErrors (M.fromList [("A", "Object")]) `shouldBe` []
      it "should have no errors if a graph forms a line" $
        checkAcyclicErrors (M.fromList [("A", "Object"), ("B", "A")]) `shouldBe` []
      it "should have no errors if a graph forms a line" $
        testPath
          classGraph
          (do put (AcyclicClassState S.empty (S.fromList ["A"]) S.empty)
              return "D")
          S.empty `shouldBe`
        CyclicPath (S.fromList ["A", "D"])
      it "should parse only A" $
        testPath
          classGraph
          (do put (AcyclicClassState S.empty S.empty S.empty)
              return "A")
          S.empty `shouldBe`
        AcyclicPath (S.fromList ["A"])
      it "should parse only A and B" $
       testPath
         (M.fromList [("A", "Object"), ("B", "A"), ("C", "B")])
         (do put (AcyclicClassState S.empty S.empty S.empty)
             return "C")
         S.empty `shouldBe`
       AcyclicPath (S.fromList ["A", "B", "C"])
      it "should have all classes identified if there a class is analyze as a table" $
        testState classGraph acyclicAnalyzerBody `shouldBe` AcyclicClassState (M.keysSet classGraph) S.empty S.empty
  where
    classGraph = M.fromList [("A", "Object"), ("B", "A"), ("C", "B"), ("D", "A")]

toClassInheritanceMap :: String -> ClassAnalysisResult
toClassInheritanceMap = createTypeMap . stringToAST programParser

testPath :: ClassInheritanceGraph -> AcyclicClassChecker Type -> S.Set Type -> Path
testPath classInheritanceGraph classAnalyzer visitedNodes =
  testState' evalState classInheritanceGraph (checkPath classAnalyzer visitedNodes)

computeState :: ClassInheritanceGraph -> AcyclicClassChecker a -> State AcyclicClassState a
computeState classGraph classAnalyzer = fst <$> runWriterT (runReaderT classAnalyzer classGraph)

testState' ::
     (State AcyclicClassState a -> AcyclicClassState -> b) -> ClassInheritanceGraph -> AcyclicClassChecker a -> b
testState' f classGraph classAnalyzer =
  let unseenNodes = (S.fromList . M.keys) classGraph
  in f (computeState classGraph classAnalyzer) (AcyclicClassState S.empty S.empty unseenNodes)

testState :: ClassInheritanceGraph -> AcyclicClassChecker a -> AcyclicClassState
testState = testState' execState
