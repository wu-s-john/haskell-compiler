module Lexer.Token
  ( Token(..)
  , Position(..)
  ) where

data Position = Position
  { getLine :: Int
  , getCol :: Int
  } deriving (Eq, Show, Read)

data Token
  = StringLiteral { getStrVal :: String
                  , getPosition :: Position }
  | IntegerLiteral { getValue :: Int
                   , getPosition :: Position }
  | TypeIdentifier { getName :: String
                   , getPosition :: Position }
  | ObjectIdentifier { getName :: String
                     , getPosition :: Position }
  -- keywords:
  | ClassKeyword { getPosition :: Position }
  | InheritsKeyword { getPosition :: Position }
  | IfKeyword { getPosition :: Position }
  | ThenKeyword { getPosition :: Position }
  | FiKeyword { getPosition :: Position }
  | ElseKeyword { getPosition :: Position }
  | TrueKeyword { getPosition :: Position }
  | FalseKeyword { getPosition :: Position }
  | NotKeyword { getPosition :: Position }
  | IsvoidKeyword { getPosition :: Position }
  | LetKeyword { getPosition :: Position }
  | InKeyword { getPosition :: Position }
  | LoopKeyword { getPosition :: Position }
  | PoolKeyword { getPosition :: Position }
  | WhileKeyword { getPosition :: Position }
  | CaseKeyword { getPosition :: Position }
  | EsacKeyword { getPosition :: Position }
  | NewKeyword { getPosition :: Position }
  | OfKeyword { getPosition :: Position }
  -- operator
  | PlusOperator { getPosition :: Position }
  | MinusOperator { getPosition :: Position }
  | TimesOperator { getPosition :: Position }
  | DivideOperator { getPosition :: Position }
  | LeftBracesOperator { getPosition :: Position }
  | LessThanOperator { getPosition :: Position }
  | LessThanOrEqualOperator { getPosition :: Position }
  | RightBracesOperator { getPosition :: Position }
  | LeftCurlyBracesOperator { getPosition :: Position }
  | RightCurlyBracesOperator { getPosition :: Position }
  | LeftParenthesesOperator { getPosition :: Position }
  | RightParenthesesOperator { getPosition :: Position }
  | AssignmentOperator { getPosition :: Position }
  | IsEqualsOperator { getPosition :: Position }
  | TypeBoundOperator { getPosition :: Position }
  | ColonOperator { getPosition :: Position }
  | SemicolonOperator { getPosition :: Position }
  | PeriodOperator { getPosition :: Position }
  | CommaOperator { getPosition :: Position }
  | AtOperator { getPosition :: Position }
  | TildeOperator { getPosition :: Position }
  -- eof
  | EOF
  -- error
  -- -- strings
  | InvalidCharacterError { getCharacterError :: Char
                          , getPosition :: Position }
  | UnterminatedStringError { getPosition :: Position }
  | EOFStringError { getPosition :: Position }
  | NullCharacterError { getPosition :: Position }
  -- -- comments
  | UnmatchedComment { getPosition :: Position }
  deriving (Eq, Show, Read)
