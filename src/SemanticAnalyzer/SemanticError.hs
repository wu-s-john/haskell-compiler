{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.SemanticError where

import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Type (Type(..))

data SemanticError
  = NonIntArgumentsPlus { left :: Type
                        , right :: Type }
  | UndeclaredIdentifier Identifier
  | LetUndefinedDeclareType { letVariableName :: Identifier
                            , declaredType :: Type }
  | WrongSubtypeLet { letVariableName :: Identifier
                    , expressionType :: Type
                    , declaredType :: Type }
  | UndefinedMethod { methodName :: Identifier }
  | DispatchUndefinedClass { className :: Type }
  | WrongNumberParameters { methodName :: Identifier }
  | WrongParameterType { methodName :: Identifier
                       , parameterName :: Identifier
                       , formalType :: Type
                       , expressionType :: Type }
  | UndefinedNewType { typeName :: String }
  | UndefinedStaticDispatch { undefinedClassName :: String }
  | WrongStaticDispatch { expressionType :: Type
                        , declaredType :: Type }
  | AttributeUndefinedDeclareType { attributeName :: Identifier
                                  , declaredType :: Type }
  | WrongSubtypeAttribute { attributeName :: Identifier
                          , expressionType :: Type
                          , declaredType :: Type }
  | UndefinedParameterType { parameterName :: Identifier
                           , formalType :: Type }
  | UndefinedReturnType { methodName :: Identifier
                        , returnType :: Type }
  | WrongSubtypeMethod { methodName :: Identifier
                       , expressionType :: Type
                       , returnType :: Type }
  deriving (Show, Eq)

type UndefinedTypeReporter = Identifier -> Type -> SemanticError

type MismatchSubtypeReporter = Identifier -> Type -> Type -> SemanticError

type IntroducedVariableReporter = (UndefinedTypeReporter, MismatchSubtypeReporter)
