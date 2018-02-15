{-# OPTIONS_GHC -Wall #-}

module Lexer.Util where

import qualified Data.List as L

import Text.Parsec.String (Parser, parseFromFile)

import Test.HUnit.Lang (assertFailure)
import Test.Hspec (Expectation, shouldBe)

import Text.Parsec.Error (ParseError, errorMessages, messageString)
import Text.Parsec.Prim (parse)

import Lexer.TokenData

testParser :: (Eq a, Show a) => Parser a -> String -> a -> Expectation
testParser parser input = checkParseResult (parse parser "title" input)

testParseFile :: Parser [Token] -> String -> String -> Expectation
testParseFile parser testFileName outputFileName = do
  outputFile <- readFile outputFileName
  let outputStrings = lines outputFile
  let correctTokens = map Right [read outputString | outputString <- outputStrings]
  parseResult <- parseFromFile parser testFileName
  checkParseResult parseResult correctTokens

--  directoryName <- getCurrentDirectory
--  print directoryName
checkParseResult :: (Eq a, Show a) => Either ParseError a -> a -> Expectation
checkParseResult parseResult =
  case parseResult of
    Left parseError ->
      \_ -> assertFailure $ "Could not parse string. Got the following errors: \n" ++ getErrorMessage parseError
    Right value -> (value `shouldBe`)

testInvalidParser :: (Show a) => Parser a -> String -> Expectation
testInvalidParser parser input =
  case parse parser "title" input of
    Left parseError -> putStrLn $ getErrorMessage parseError
    Right value -> assertFailure $ "Expecting parser to fail. Instead it succeeded with the following " ++ show value

getErrorMessage :: ParseError -> String
getErrorMessage parseError = L.intercalate "\n" $ map messageString (errorMessages parseError)

--parseSequence :: [Parser Token] -> Parser [Token]
--parserList parserList fileName = foldl  ()
