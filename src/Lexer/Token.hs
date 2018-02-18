module Lexer.Token
  ( Token(..)
  ) where


data Token
  =
    StringLiteral String
  | IntegerLiteral Int
  | TypeIdentifier String
  | ObjectIdentifier String
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
  | EOF
  -- error
  -- -- strings
  | InvalidCharacterError Char
  | UnterminatedStringError
  | EOFStringError
  | NullCharacterError
  -- -- comments
  | UnmatchedComment
  deriving (Eq, Show, Read)
