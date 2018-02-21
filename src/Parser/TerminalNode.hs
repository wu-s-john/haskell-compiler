{-# OPTIONS_GHC -Wall #-}

module Parser.TerminalNode where

data BinaryOpTerminal
  = PlusTerminal
  | MinusTerminal
  | TimesTerminal
  | DivideTerminal
  | LessThanTerminal
  | LessThanOrEqualTerminal
  | EqualTerminal
  deriving (Show, Read, Eq)

data UnaryOpTerminal
  = IsvoidTerminal
  | NotTerminal
  | TildeTerminal
  deriving (Show, Read, Eq)


newtype Identifier = Identifier String deriving (Show, Read, Eq)
newtype Type = Type String deriving (Show, Read, Eq)
