{-# OPTIONS_GHC -Wall #-}

module Parser.ParserUtil where

import Data.List (find)

import Lexer.Lexer (runAlex, scanner)
import Lexer.Token

scanErrors :: [Token] -> [Token]
scanErrors tokens =
  case find classifyErrorToken tokens of
    Just errorToken -> error $ "Found error token" ++ show errorToken
    Nothing -> tokens
  where
    classifyErrorToken (InvalidCharacterError _ _) = True
    classifyErrorToken (UnterminatedStringError _) = True
    classifyErrorToken (EOFStringError _) = True
    classifyErrorToken (NullCharacterError _) = True
    classifyErrorToken _ = False

stringToAST :: ([Token] -> a) -> String -> a
stringToAST parser code = parser $
  case code `runAlex` scanner of
    Right tokens -> tokens
