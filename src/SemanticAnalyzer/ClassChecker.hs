{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.ClassChecker where

import Control.Monad.Loops (whileM_)
import Control.Monad.RWS.Lazy (RWS, execRWS)
import Control.Monad.Reader (ask)
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Classes
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.AST (Class(Class), Program(Program))
import qualified Parser.TerminalNode as T
import SemanticAnalyzer.ClassRelationshipError
import SemanticAnalyzer.PrimitiveTypes (primitiveTypes)

type ClassInheritanceGraph = M.Map T.Type T.Type

type ClassGraphBuilder = Writer [ClassRelationshipError] ClassInheritanceGraph

type GraphCheckerResult = Either [ClassRelationshipError] ClassInheritanceGraph

data AcyclicClassState = AcyclicClassState
  { getVisitedNodes :: [T.Type]
  , getCycleNodes :: [T.Type]
  } deriving (Show, Eq)

type AcyclicClassChecker = RWS ClassInheritanceGraph [ClassRelationshipError] AcyclicClassState

data Path
  = CyclicPath [T.Type]
  | AcyclicPath [T.Type]
  deriving (Show, Eq)

createClassGraph :: Program -> ClassGraphBuilder
createClassGraph (Program classList) = foldM addClass M.empty classList
  where
    addClass classInheritanceMap (Class className inheritName _) =
      case M.lookup className classInheritanceMap of
        Just _ -> tell [PreviouslyDefined className] >> return classInheritanceMap
        Nothing -> return $ M.insert className inheritName classInheritanceMap

checkIllegalInheritance :: ClassInheritanceGraph -> [ClassRelationshipError]
checkIllegalInheritance graph = foldr buildErrors [] (M.toList graph)
  where
    buildErrors (className, parentName) accList
      | checkPrimitiveInheritance parentName = PrimitiveInheritance className parentName : accList
      | M.notMember parentName graph && parentName /= "Object" = UndefinedInheritance className parentName : accList
      | otherwise = accList

checkPrimitiveInheritance :: T.Type -> Bool
checkPrimitiveInheritance parentName = parentName `elem` "SELF_TYPE" : primitiveTypes

checkAcyclicErrors :: ClassInheritanceGraph -> [ClassRelationshipError]
checkAcyclicErrors classInheritanceGraph = snd $ execRWS acyclicAnalyzer classInheritanceGraph (AcyclicClassState [] [])

acyclicAnalyzer :: AcyclicClassChecker ()
acyclicAnalyzer = do
  classGraph <- ask
  let nodeSet = M.keys classGraph
  whileM_
    (do (AcyclicClassState visitedNodes cyclicNodes) <- get
        return $ not $ S.fromList (visitedNodes ++ cyclicNodes) `eq1` S.fromList nodeSet)
    acyclicAnalyzerBody

acyclicAnalyzerBody :: AcyclicClassChecker ()
acyclicAnalyzerBody = do
  classGraph <- ask
  (AcyclicClassState visitedNodes cyclicNodes) <- get
  let class' = head $ M.keys classGraph L.\\ (visitedNodes ++ cyclicNodes)
  pathResult <- checkPath (return class') []
  case pathResult of
    CyclicPath newCyclicClasses -> put (AcyclicClassState visitedNodes (cyclicNodes ++ newCyclicClasses))
    AcyclicPath discoveredClasses' -> put (AcyclicClassState (visitedNodes ++ discoveredClasses') cyclicNodes)

checkPath :: AcyclicClassChecker T.Type -> [T.Type] -> AcyclicClassChecker Path
checkPath classChecker classPath = do
  classGraph <- ask
  (AcyclicClassState visitedNodes cycleNodes) <- get
  currentClass <- classChecker
  case currentClass of
    "Object" -> return $ AcyclicPath classPath
    class'
      | class' `elem` visitedNodes -> return $ AcyclicPath classPath
      | class' `elem` classPath -> do
        tell [InheritanceCycle class']
        return $ CyclicPath classPath
      | class' `elem` cycleNodes -> return $ CyclicPath classPath
      | otherwise -> checkParentPath classGraph currentClass
  where
    checkParentPath classGraph currentClass = do
      subresult <- checkPath (return (classGraph M.! currentClass)) (currentClass : classPath)
      case subresult of
        CyclicPath parentPath ->
          when (currentClass `notElem` parentPath) (tell [InheritanceCycle currentClass]) >>
          return (CyclicPath parentPath)
        AcyclicPath parentPath -> return (AcyclicPath parentPath)

checkAndVerifyClassGraph :: Program -> GraphCheckerResult
checkAndVerifyClassGraph program =
  let (graph, previouslyDefinedClassErrors) = runWriter (createClassGraph program)
      errors = previouslyDefinedClassErrors ++ checkIllegalInheritance graph
  in case errors of
       [] -> findCyclicErrors graph
       _ -> Left errors
  where
    findCyclicErrors graph =
      case checkAcyclicErrors graph of
        [] -> Right graph
        errors -> Left errors
