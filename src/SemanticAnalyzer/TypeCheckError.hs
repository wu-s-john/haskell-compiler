{-# OPTIONS_GHC -Wall #-}

module SemanticAnalyzer.TypeCheckError where

import Parser.TerminalNode (Identifier)
import SemanticAnalyzer.Type (Type(..))

data TypeCheckError
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
  | UndefinedNewType { typeName :: Type }
  | UndefinedStaticDispatch { undefinedClassName :: Type }
  | WrongSubtypeStaticDispatch { expressionType :: Type
                               , staticType :: Type }
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

type UndefinedTypeReporter = Type -> TypeCheckError

type MismatchSubtypeReporter = Type -> Type -> TypeCheckError

type IntroducedVariableReporter = (String -> UndefinedTypeReporter, String -> MismatchSubtypeReporter)

type SubtypeReporter = (Type -> TypeCheckError, Type -> Type -> TypeCheckError)
