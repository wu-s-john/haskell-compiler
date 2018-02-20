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
@reservedOps = "[" | "]" | "{" | "}" | "(" |  ")" | "<" | "<="| "<-" | "=" | ":" | ";" | "." | "," | "@" | "~" | "+"| "-"| "*"| "/"| "=>"

tokens :-

  <0> $white+				;
  <0> "--".*				;
  <0> "*)"				    { toSingularToken UnmatchedComment }
  <0> "(*" .* "*)"          ;
  <0> @reservedOps          { toParameterizedToken toOperator }
  <0> [0-9]+			    { toParameterizedToken (IntegerLiteral . read) }
  <0> @typeIdentifier       { toParameterizedToken (treatKeyword TypeIdentifier) }
  <0> @objectIdentifier     { toParameterizedToken (treatKeyword ObjectIdentifier) }

  <0> \" @string* \"        { toParameterizedToken toStringToken } -- todo remove transform
  <0> \" @string* \n        { toSingularToken UnterminatedStringError }
  <0> \" @string*           { toSingularToken EOFStringError }

  <0> .                     { toParameterizedToken (InvalidCharacterError . head) }
{

toParameterizedToken :: (String -> Position -> Token) -> AlexInput -> Int -> Alex Token
toParameterizedToken tokenFactory ((AlexPn _ lineNumber colNumber), _, _, str) len = let stringToken = take len str in
                                                       return (tokenFactory stringToken (Position lineNumber colNumber))

toSingularToken :: (Position -> Token) -> AlexInput -> Int -> Alex Token
toSingularToken posTokenCB ((AlexPn _ lineNumber colNumber), _, _, _) _ = return (posTokenCB (Position lineNumber colNumber))

-- The token type:

scanner :: Alex [Token]
scanner = do
    token <- alexMonadScan
    case token of
        EOF ->  return []
        _ -> do subresult <- scanner; return (token : subresult)

alexEOF = return $ EOF-- todo get the size fo file
}
