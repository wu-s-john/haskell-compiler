{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.ClassChecker where

import Control.Monad.Loops
import Control.Monad.State
import qualified Data.Map as M
import Parser.AST (Class(Class), Program(Program))
import Parser.TerminalNode

data ClassError
  = UndefinedInheritance String
                         String
  | PrimitiveInheritance String String
  | InheritanceCycle String
  | PreviouslyDefined String
  deriving (Show, Eq)

type ClassInheritanceGraph = M.Map Type Type

type ClassInheritanceState = ([ClassError], ClassInheritanceGraph)

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
  where buildErrors (className, parentName) accList
          | checkPrimitiveInheritance parentName = PrimitiveInheritance className parentName : accList
          | M.notMember parentName graph = UndefinedInheritance className parentName : accList
          | otherwise = accList

checkPrimitiveInheritance :: Type -> Bool
checkPrimitiveInheritance parentName = parentName `elem` ["Bool", "String", "Integer", "SELF_TYPE"]

