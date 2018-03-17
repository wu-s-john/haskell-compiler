{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticAnalyzer where

import qualified Data.Map as M
import qualified Parser.TerminalNode as T
import Control.Monad.Reader (ReaderT)
import SemanticAnalyzer.ClassEnvironment (ClassEnvironment)
import Control.Monad.Writer (WriterT)
import Control.Monad.State (State)

data SemanticError
  = NonIntArgumentsPlus { left :: T.Type
                        , right :: T.Type }
  | UndeclaredIdentifier T.Identifier
  | MismatchDeclarationType { inferredType :: T.Type
                            , declaredType :: T.Type }
  | UndefinedMethod { methodName :: T.Identifier }
  | DispatchUndefinedClass { className :: T.Type }
  | WrongNumberParameters { methodName :: T.Identifier }
  | WrongParameterType { methodName :: T.Identifier
                       , parameterName :: T.Identifier
                       , formalType :: T.Type
                       , expressionType :: T.Type }
  deriving (Show, Eq)


type ObjectEnvironment = M.Map T.Identifier T.Type
type SemanticAnalyzer = ReaderT (T.Type, ClassEnvironment) (WriterT [SemanticError] (State ObjectEnvironment))
