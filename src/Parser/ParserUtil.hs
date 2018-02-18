{-# OPTIONS_GHC -Wall #-}

module Parser.ParserUtil where

import Data.List (find)

import Parser.AST
import Lexer.Lexer (runAlex, scanner)
import Lexer.Token
import Parser.Parser (coolParser)

scanErrors :: [Token] -> [Token]
scanErrors tokens =
  case find classifyErrorToken tokens of
    Just errorToken -> error $ "Found error token" ++ show errorToken
    Nothing -> tokens
  where
    classifyErrorToken (InvalidCharacterError _) = True
    classifyErrorToken UnterminatedStringError = True
    classifyErrorToken EOFStringError = True
    classifyErrorToken NullCharacterError = True
    classifyErrorToken _ = False

parseCode :: String -> Class
parseCode code = coolParser $
  case code `runAlex` scanner of
    Right tokens -> tokens
