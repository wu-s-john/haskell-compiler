{
module Parser.Parser where
import Data.Char
import Lexer.Token
import Parser.AST
import Data.Maybe
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
      ':'             { ColonOperator }
      '<-'            { AssignmentOperator }
      objectID        { ObjectIdentifier $$ }
      typeID          { TypeIdentifier $$ }

%left '+' '-'
%left '*' '/'
%%

feat :: { Feature }
feat :
        objectID ':' typeID opt_expr     { Attribute (Identifier $1) (Type $3) $4}

opt_expr :: {Maybe Expression}
opt_expr : {- empty -}            { Nothing }
         | '<-' expr              { Just $2 }

expr :: { Expression }
expr  :
        expr '+' expr           { BinaryOp PlusOperator $1 $3 }
      | expr '-' expr           { BinaryOp MinusOperator $1 $3 }
      | expr '*' expr           { BinaryOp TimesOperator $1 $3 }
      | expr '/' expr           { BinaryOp DivideOperator $1 $3 }
      | int                     { IntegerExpr $1 }
      | objectID                { IdentifierExpr $1}
      | '(' expr ')'            { $2 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

}
