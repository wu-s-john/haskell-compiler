{-# OPTIONS_GHC -Wall #-}

module Parser.TerminalNode where

data BinaryOpTerminal = PlusTerminal | MinusTerminal | TimesTerminal | DivideTerminal
  deriving (Show, Read, Eq)

