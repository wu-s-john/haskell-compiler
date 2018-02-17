{-# OPTIONS_GHC -Wall #-}

module Parser.ParserUtil where

import Parser.AST
import Lexer.Lexer (runAlex, scanner)
import Parser.Parser (coolParser)

parseCode :: String -> Feature
parseCode code = coolParser $
  case code `runAlex` scanner of
    Right tokens -> tokens
