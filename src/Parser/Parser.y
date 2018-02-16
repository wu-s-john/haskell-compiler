{
module Parser.Parser where
import Data.Char
import Lexer.Token
import Parser.AST
}


%name coolParser
%tokentype { Token }
%error { parseError }


%token
      int             { IntegerToken $$ }
      '+'             { PlusOperator }
      '-'             { MinusOperator }
      '*'             { TimesOperator }
      '/'             { DivideOperator }
      '('             { LeftParenthesesOperator }
      ')'             { RightParenthesesOperator }

%left '+' '-'
%left '*'
%left '/'
%%

Exp :: { Expression }
Exp  :
        Exp '+' Exp           { BinaryOp PlusOperator $1 $3 }
      | Exp '-' Exp           { BinaryOp MinusOperator $1 $3 }
      | Exp '*' Exp           { BinaryOp TimesOperator $1 $3 }
      | Exp '/' Exp           { BinaryOp DivideOperator $1 $3 }
      | int                   { IntegerExpr $1 }
      | '(' Exp ')'           { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
