{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

module Parser.ParserUtil where

import Data.List (find)

import Lexer.Lexer (runAlex, scanner)
import Lexer.Token
import Parser.AST
import Parser.Parser
       (classParser, expressionParser, featureParser, featuresParser,
        programParser)

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
stringToAST parser code =
  parser $
  case code `runAlex` scanner of
    Right tokens -> tokens

class Parsable a where
  parse :: String -> a

instance Parsable Program where
  parse = stringToAST programParser

instance Parsable Class where
  parse = stringToAST classParser

instance Parsable Feature where
  parse = stringToAST featureParser

instance Parsable Expression where
  parse = stringToAST expressionParser

instance Parsable [Feature] where
  parse = stringToAST featuresParser
