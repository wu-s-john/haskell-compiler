{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzer where

import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.State (State)
import Control.Monad.Writer (WriterT)
import qualified Data.Map as M
import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Class (ClassRecord)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import SemanticAnalyzer.Type (Type(..))

data SemanticError
  = NonIntArgumentsPlus { left :: Type
                        , right :: Type }
  | UndeclaredIdentifier Identifier
  | MismatchDeclarationType { inferredType :: Type
                            , declaredType :: Type }
  | UndefinedMethod { methodName :: Identifier }
  | DispatchUndefinedClass { className :: Type }
  | WrongNumberParameters { methodName :: Identifier }
  | WrongParameterType { methodName :: Identifier
                       , parameterName :: Identifier
                       , formalType :: Type
                       , expressionType :: Type }
  | UndefinedNewType { typeName :: String }
  deriving (Show, Eq)

type ObjectEnvironment = M.Map Identifier Type

type SemanticAnalyzer = ReaderT (String, ClassEnvironment) (WriterT [SemanticError] (State ObjectEnvironment))

lookupClass :: String -> SemanticAnalyzer (Maybe ClassRecord)
lookupClass typeString = do
  (_, classEnvironment) <- ask
  return $ typeString `M.lookup` classEnvironment
