{-# OPTIONS_GHC -Wall #-}

module Lexer.LexerUtil where

import Lexer.Lexer
import Lexer.Token (Token(..))
import Data.List (intercalate)

parseFile :: Alex [Token] -> String -> IO (Either String [Token])
parseFile lexer fileName = do
  fileContents <- readFile fileName
  return  $ runAlex fileContents lexer


writeResults :: String -> [Token] -> IO ()
writeResults fileName tokens =
  let fileContents = intercalate "\n" $ map show tokens in
  writeFile fileName fileContents


writeScannerResults :: String -> String -> IO()
writeScannerResults fileName resultsName = do
  results <- parseFile  scanner fileName
  case results of
    Right tokens -> writeResults resultsName tokens
    Left _ -> error "Could not parse results"
