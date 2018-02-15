module Lexer.Token
  ( Token(..)
  ) where

data Token
  =
    StringToken String
  | IntegerToken Int
  | TypeIdentifier String
  | ObjectIdentifier String
  -- special
  | SelfToken
  | SELF_TYPE_TOKEN
  -- keywords
  | ClassKeyword
  | InheritsKeyword
  | IfKeyword
  | ThenKeyword
  | FiKeyword
  | ElseKeyword
  | TrueKeyword
  | FalseKeyword
  | NotKeyword
  | IsvoidKeyword
  | LetKeyword
  | InKeyword
  | LoopKeyword
  | PoolKeyword
  | WhileKeyword
  | CaseKeyword
  | EsacKeyword
  | NewKeyword
  | OfKeyword
  -- operator
  | PlusOperator
  | MinusOperator
  | TimesOperator
  | DivideOperator
  | LeftBracesOperator
  | LessThanOperator
  | LessThanOrEqualOperator
  | RightBracesOperator
  | LeftCurlyBracesOperator
  | RightCurlyBracesOperator
  | LeftParenthesesOperator
  | RightParenthesesOperator
  | AssignmentOperator
  | IsEqualsOperator
  | TypeBoundOperator
  | ColonOperator
  | SemicolonOperator
  | PeriodOperator
  | CommaOperator
  | AtOperator
  | TildeOperator
  -- eof
  | EOFToken
  -- error
  -- -- strings
  | InvalidCharacterErrorToken Char
  | UnterminatedStringErrorToken
  | EOFStringErrorToken
  | NullCharacterErrorToken
  -- -- comments
  | UnmatchedCommentToken
  deriving (Eq, Show, Read)
