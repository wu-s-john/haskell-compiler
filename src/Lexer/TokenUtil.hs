{-# OPTIONS_GHC -Wall #-}

module Lexer.TokenUtil where

import Data.Char (toLower)
import Data.List (isInfixOf)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)

import Lexer.Token (Position, Token(..))

removeFirstLast :: [a] -> [a]
removeFirstLast xs@(_:_) = tail (init xs)
removeFirstLast _ = []

convertSpecialEscapeCharacters :: String -> String
convertSpecialEscapeCharacters ('\\':anyChar:list) = convertEscape anyChar : convertSpecialEscapeCharacters list
  where
    convertEscape newChar =
      case newChar of
        'b' -> '\b'
        't' -> '\t'
        'n' -> '\n'
        'f' -> '\f'
        '\n' -> '\n'
        _ -> newChar
convertSpecialEscapeCharacters (headList:tailList) = headList : convertSpecialEscapeCharacters tailList
convertSpecialEscapeCharacters [] = []

toStringToken :: String -> Position -> Token
toStringToken string position =
  case throwNullCharacterToken (removeFirstLast string) position of
    (NullCharacterError _) -> NullCharacterError position
    (StringLiteral processedString _) -> StringLiteral (convertSpecialEscapeCharacters processedString) position
    _ -> error "Could not match token to a corresponding string Token"

throwNullCharacterToken :: String -> (Position -> Token)
throwNullCharacterToken string =
  if "\\0" `isInfixOf` string
    then NullCharacterError
    else StringLiteral string

stringKeywordMap :: Map String (Position -> Token)
stringKeywordMap =
  M.fromList
    [ ("class", ClassKeyword)
    , ("inherits", InheritsKeyword)
    , ("if", IfKeyword)
    , ("then", ThenKeyword)
    , ("fi", FiKeyword)
    , ("else", ElseKeyword)
    , ("true", TrueKeyword)
    , ("false", FalseKeyword)
    , ("not", NotKeyword)
    , ("isvoid", IsvoidKeyword)
    , ("let", LetKeyword)
    , ("in", InKeyword)
    , ("loop", LoopKeyword)
    , ("pool", PoolKeyword)
    , ("while", WhileKeyword)
    , ("case", CaseKeyword)
    , ("esac", EsacKeyword)
    , ("new", NewKeyword)
    , ("of", OfKeyword)
    ]

treatKeyword :: (String -> Position -> Token) -> String -> Position -> Token
treatKeyword tokenFactory identifier =
  let keywordLookup = calculateKeyLookup identifier
  in fromMaybe (tokenFactory identifier) (M.lookup keywordLookup stringKeywordMap)

calculateKeyLookup :: String -> String
calculateKeyLookup identifier@(headIdentifier:tailIdentifier) =
  let lowerCaseIdentifier = map toLower identifier
  in if "true" == lowerCaseIdentifier || "false" == lowerCaseIdentifier
       then headIdentifier : map toLower tailIdentifier
       else lowerCaseIdentifier
calculateKeyLookup [] = []

stringOperatorMap :: Map String (Position -> Token)
stringOperatorMap =
  M.fromList
    [ ("[", LeftBracesOperator)
    , ("]", RightBracesOperator)
    , ("{", LeftCurlyBracesOperator)
    , ("}", RightCurlyBracesOperator)
    , ("(", LeftParenthesesOperator)
    , (")", RightParenthesesOperator)
    , ("<", LessThanOperator)
    , ("<=", LessThanOrEqualOperator)
    , ("<-", AssignmentOperator)
    , ("=", IsEqualsOperator)
    , (":", ColonOperator)
    , (";", SemicolonOperator)
    , (".", PeriodOperator)
    , (",", CommaOperator)
    , ("@", AtOperator)
    , ("~", TildeOperator)
    , ("+", PlusOperator)
    , ("-", MinusOperator)
    , ("*", TimesOperator)
    , ("/", DivideOperator)
    , ("=>", TypeBoundOperator)
    ]

toOperator :: String -> Position -> Token
toOperator operator = stringOperatorMap M.! operator
