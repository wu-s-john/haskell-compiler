{
module Lexer.Lexer  where
import Lexer.Token
import Lexer.TokenUtil
}

%wrapper "monad"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters
$alphaNumeric = [a-zA-Z0-9] -- alphanumeric characters


@endingComment = \*\)

@validIdentifier = "_" | $alphaNumeric
@string = [^\"\n] | \\\" | \\\n
@typeIdentifier = [A-Z] @validIdentifier*
@objectIdentifier = [a-z] @validIdentifier*
@reservedOps = "[" | "]" | "{" | "}" | "(" |  ")" | "<" | "<="| "<-" | "=" | ":" | ";" | "." | "," | "@" | "~" | "+"| "-"| "*"| "\\"| "=>"

tokens :-

  <0> $white+				;
  <0> "--".*				;
  <0> "*)"				    { toSingularToken UnmatchedCommentToken }
  <0> "(*"                  { begin comment }
  <comment> .               { skip  }
  <comment> "*)"            { begin 0 }
  <0> @reservedOps          { toParameterizedToken toOperator }
  <0> [0-9]+			    { toParameterizedToken (IntegerToken . read) }
  <0> @typeIdentifier       { toParameterizedToken (treatKeyword TypeIdentifier) }
  <0> @objectIdentifier     { toParameterizedToken (treatKeyword ObjectIdentifier) }

  <0> \" @string* \"        { toParameterizedToken toStringToken } -- todo remove transform
  <0> \" @string* \n        { toSingularToken UnterminatedStringErrorToken }
  <0> \" @string*           { toSingularToken EOFStringErrorToken }

  <0> .                     { toParameterizedToken (InvalidCharacterErrorToken . head) }
{
-- Each action has type :: String -> Token

toParameterizedToken :: (String -> Token) -> AlexInput -> Int -> Alex Token
toParameterizedToken tokenFactory (_, _, _, str) len = let stringToken = take len str in
                                                       return $ tokenFactory stringToken\

toSingularToken :: Token -> AlexInput -> Int -> Alex Token
toSingularToken token _ _ = return token

-- The token type:

scanner :: Alex [Token]
scanner = do
    token <- alexMonadScan
    if token == EOFToken
       then return []
       else do subresult <- scanner; return (token : subresult);

identifyEOF :: user -> AlexInput -> Int -> AlexInput -> Bool
identifyEOF _ _ _ (_, _, _, after) =
  case after of
    []       -> True  -- end-of-file
    _        -> False

alexEOF = return EOFToken
}
