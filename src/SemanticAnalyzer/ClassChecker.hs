{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleContexts #-}

module SemanticAnalyzer.ClassChecker where

import Control.Monad.Loops (whileM_)
import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State
import Control.Monad.Writer
import Data.Functor.Classes
import qualified Data.Map as M
import qualified Data.Set as S
import Parser.AST (Class(Class), Program(Program))
import Parser.TerminalNode

data ClassError
  = UndefinedInheritance String
                         String
  | PrimitiveInheritance String
                         String
  | InheritanceCycle String
  | PreviouslyDefined String
  deriving (Show, Eq)

type ClassInheritanceGraph = M.Map Type Type

type ClassInheritanceState = ([ClassError], ClassInheritanceGraph)

data AcyclicClassState = AcyclicClassState
  { getVisitedNodes :: S.Set Type
  , getCycleNodes :: S.Set Type
  , getUnseenNodes :: S.Set Type
  } deriving (Show, Eq)

type AcyclicClassChecker = ReaderT ClassInheritanceGraph (WriterT [ClassError] (State AcyclicClassState))

data Path
  = CyclicPath (S.Set Type)
  | AcyclicPath (S.Set Type)
  deriving (Show, Eq)

acyclicAnalyzer :: AcyclicClassChecker ()
acyclicAnalyzer = do
  classGraph <- ask
  let nodeSet = S.fromList $ M.keys classGraph
  whileM_
    (do (AcyclicClassState visitedNodes cyclicNodes unseenNodes) <- get
        return $ not $ (visitedNodes `S.union` cyclicNodes) `eq1` nodeSet)
    acyclicAnalyzerBody

acyclicAnalyzerBody :: AcyclicClassChecker ()
acyclicAnalyzerBody = do
  (AcyclicClassState visitedNodes cyclicNodes unseenNodes) <- get
  let randomClass = S.findMax unseenNodes
  pathResult <- checkPath (return randomClass) S.empty
  case pathResult of
    CyclicPath newCyclicClasses ->
      put
        (AcyclicClassState
           visitedNodes
           (cyclicNodes `S.union` newCyclicClasses)
           (unseenNodes `S.difference` newCyclicClasses))
    AcyclicPath discoveredClasses' ->
      put
        (AcyclicClassState
           (visitedNodes `S.union` discoveredClasses')
           cyclicNodes
           (unseenNodes `S.difference` discoveredClasses'))

checkAcyclicErrors :: ClassInheritanceGraph -> [ClassError]
checkAcyclicErrors classInheritanceGraph =
  let unseenNodes = (S.fromList . M.keys) classInheritanceGraph
  in evalState
       (execWriterT $ runReaderT acyclicAnalyzer classInheritanceGraph)
       (AcyclicClassState S.empty S.empty unseenNodes)

checkPath :: AcyclicClassChecker Type -> S.Set Type -> AcyclicClassChecker Path
checkPath classChecker classPath = do
  classGraph <- ask
  (AcyclicClassState visitedNodes cycleNodes _) <- get
  currentClass <- classChecker
  case currentClass of
    "Object" -> return $ AcyclicPath classPath
    class'
      | class' `S.member` visitedNodes -> return $ AcyclicPath (S.insert class' visitedNodes)
      | class' `S.member` cycleNodes -> do
        tell [InheritanceCycle class']
        return $ CyclicPath (class' `S.insert` classPath)
      | otherwise -> checkPath (return (classGraph M.! currentClass)) (S.insert currentClass classPath)
  where
    reportError subResult className = do
      path <- subResult
      case path of
        CyclicPath _ -> do
          tell [InheritanceCycle className]
          subResult
        _ -> subResult

data ClassAnalysisResult
  = Error [ClassError]
          ClassInheritanceGraph
  | OK ClassInheritanceGraph
  deriving (Show, Eq)

createTypeMapState :: Program -> State ClassInheritanceState ()
createTypeMapState (Program classList) =
  forM_
    classList
    (\(Class className inheritName _) -> do
       (errors, classInheritanceMap) <- get
       case M.lookup className classInheritanceMap of
         Just _ -> put (PreviouslyDefined className : errors, classInheritanceMap)
         Nothing -> put (errors, M.insert className inheritName classInheritanceMap))

createTypeMapState' :: Program -> WriterT [ClassError] (State ClassInheritanceGraph) ()
createTypeMapState' (Program classList) =
  forM_
    classList
    (\(Class className inheritName _) -> do
       classInheritanceMap <- get
       case M.lookup className classInheritanceMap of
         Just _ -> tell [PreviouslyDefined className]
         Nothing -> put $ M.insert className inheritName classInheritanceMap)

runClassInheritanceState :: State ClassInheritanceState () -> ClassAnalysisResult
runClassInheritanceState classInheritanceState =
  let (errors, classInheritanceMap) = execState classInheritanceState ([], M.empty)
  in case errors of
       [] -> OK classInheritanceMap
       _ -> Error errors classInheritanceMap

createTypeMap :: Program -> ClassAnalysisResult
createTypeMap programNode = runClassInheritanceState (createTypeMapState programNode)

checkIllegalInheritance :: ClassInheritanceGraph -> [ClassError]
checkIllegalInheritance graph = foldr buildErrors [] (M.toList graph)
  where
    buildErrors (className, parentName) accList
      | checkPrimitiveInheritance parentName = PrimitiveInheritance className parentName : accList
      | M.notMember parentName graph = UndefinedInheritance className parentName : accList
      | otherwise = accList

checkPrimitiveInheritance :: Type -> Bool
checkPrimitiveInheritance parentName = parentName `elem` ["Bool", "String", "Int", "SELF_TYPE"]
