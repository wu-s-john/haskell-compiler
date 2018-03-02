{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassCheckerSpec
  ( main
  , spec
  ) where

import Control.Monad.Reader (runReaderT)
import Control.Monad.State (State, evalState, execState)
import Control.Monad.Writer (runWriter, runWriterT)
import qualified Data.Map as M
import Parser.Parser (programParser)
import Parser.ParserUtil (stringToAST)
import Parser.TerminalNode (Type)
import SemanticAnalyzer.ClassChecker
import Test.Hspec
       (Expectation, Spec, describe, hspec, it, shouldBe)

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "classChecker" $ do
    describe "createTypeMap" $ do
      it "should parse one class of a program into a Map" $
        testBuilder "class Foo {};" (M.fromList [("Foo", "Object")]) []
      it "should parse multiple classes of a program into a Map" $
        testBuilder
          "class A {}; class B inherits A {}; class C inherits B {}; class D inherits A {};"
          (M.fromList [("A", "Object"), ("B", "A"), ("C", "B"), ("D", "A")])
          []
      it "should have an error invoked if a single class occurs multiple times" $
        testBuilder "class A {}; class A inherits B {}; " (M.fromList [("A", "Object")]) [PreviouslyDefined "A"]
      it "should have an error invoked if multiple classes occurs multiple times" $
        testBuilder
          "class A inherits B {}; class A {}; class Foo {}; class Foo {}; class A {}; class A {};"
          (M.fromList [("A", "B"), ("Foo", "Object")])
          [PreviouslyDefined "A", PreviouslyDefined "Foo", PreviouslyDefined "A", PreviouslyDefined "A"]
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
      it "should have an error of an acyclic class" $
        checkAcyclicErrors (M.fromList [("B", "A"), ("C", "B"), ("A", "C")]) `shouldBe` [InheritanceCycle "A"] -- todo should get inherit errors from A, B, C
      it "should have no errors if a graph forms a line" $
        computeClassInheritancePath (return "D") (AcyclicClassState [] ["A"]) classGraph `shouldBe` CyclicPath ["D"]
      it "should parse only one class if it's parent is Object" $
        computeClassInheritancePath (return "A") (AcyclicClassState [] []) classGraph `shouldBe` AcyclicPath ["A"]
      it "should parse only a few classes if the first class is not a leaf class" $
        computeClassInheritancePath
          (return "C")
          (AcyclicClassState [] [])
          (M.fromList [("A", "Object"), ("B", "A"), ("C", "B")]) `shouldBe`
        AcyclicPath ["A", "B", "C"]
      it "should identify if there is a cycle" $
        computeClassInheritancePath (return "B") (AcyclicClassState [] []) (M.fromList [("A", "B"), ("B", "A")]) `shouldBe`
        CyclicPath ["A", "B"]
      it "should identify a cycle if one the classes' parents is a cycle" $
        computeClassInheritancePath
          (return "C")
          (AcyclicClassState [] ["A", "B"])
          (M.fromList [("A", "B"), ("B", "A"), ("C", "B")]) `shouldBe`
        CyclicPath ["C"]
      it "should have all classes identified in a legal table" $
        computeClassAnalyzer acyclicAnalyzer classGraph `shouldBe` AcyclicClassState ["A", "B", "C", "D"] []
      it "should identify cycles" $
        computeClassAnalyzer acyclicAnalyzer (M.fromList [("A", "B"), ("B", "C"), ("C", "A")]) `shouldBe`
        AcyclicClassState [] ["C", "B", "A"]
  where
    classGraph = M.fromList [("A", "Object"), ("B", "A"), ("C", "B"), ("D", "A")]

testBuilder :: String -> ClassInheritanceGraph -> [ClassError] -> Expectation
testBuilder program expectedMap expectedErrors = do
  actualMap `shouldBe` expectedMap
  actualErrors `shouldBe` expectedErrors
  where
    classGraphBuilder = toClassInheritanceMap program
    (actualMap, actualErrors) = runWriter classGraphBuilder

toClassInheritanceMap :: String -> ClassGraphBuilder
toClassInheritanceMap = createClassGraph . stringToAST programParser

computeState :: ClassInheritanceGraph -> AcyclicClassChecker a -> State AcyclicClassState a
computeState classGraph classAnalyzer = fst <$> runWriterT (runReaderT classAnalyzer classGraph)

computeClassAnalyzer :: AcyclicClassChecker a -> ClassInheritanceGraph -> AcyclicClassState
computeClassAnalyzer classChecker = runAnalyzer execState classChecker (AcyclicClassState [] [])

runAnalyzer ::
     (State AcyclicClassState a -> AcyclicClassState -> b)
  -> AcyclicClassChecker a
  -> AcyclicClassState
  -> ClassInheritanceGraph
  -> b
runAnalyzer stateRunner classAnalyzer state classGraph = stateRunner (computeState classGraph classAnalyzer) state

computeClassInheritancePath :: AcyclicClassChecker Type -> AcyclicClassState -> ClassInheritanceGraph -> Path
computeClassInheritancePath classPathChecker = runAnalyzer evalState (checkPath classPathChecker [])
