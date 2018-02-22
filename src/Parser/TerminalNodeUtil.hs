{-# OPTIONS_GHC -Wall #-}

module Parser.TerminalNodeUtil where

import Lexer.Token as T
import Parser.TerminalNode

toType :: T.Token -> Type
toType token = Type (T.getName token)

toIdentifier :: T.Token -> Identifier
toIdentifier token = Identifier (T.getName token)
