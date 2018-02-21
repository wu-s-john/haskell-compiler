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
