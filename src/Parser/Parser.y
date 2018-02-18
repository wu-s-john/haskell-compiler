{
module Parser.Parser where
import Data.Char
import Lexer.Token
import Parser.AST
import Data.Maybe
import Data.List
}


%name coolParser
%tokentype { Token }
%error { parseError }


%token
      int             { IntegerLiteral $$ }
      '+'             { PlusOperator }
      '-'             { MinusOperator }
      '*'             { TimesOperator }
      '/'             { DivideOperator }
      '('             { LeftParenthesesOperator }
      ')'             { RightParenthesesOperator }
      '{'             { LeftCurlyBracesOperator }
      '}'             { RightCurlyBracesOperator }
      ':'             { ColonOperator }
      '<-'            { AssignmentOperator }
      ';'             { SemicolonOperator}
      'class'         { ClassKeyword }
      'inherits'        { InheritsKeyword }
      objectID        { ObjectIdentifier $$ }
      typeID          { TypeIdentifier $$ }

%left '+' '-'
%left '*' '/'
%%

class :: { Class }
class :
        'class' typeID '{' feats '}'    {OrphanedClass (Type $2) $4 }
      | 'class' typeID  'inherits' typeID '{' feats '}'    {InheritedClass (Type $2) (Type $4) $6 }

feats :: { [Feature] }
feats :
        {- empty -}               { [] }
      | feat ';' feats     { $1 : $3 }

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
parseError tokens = error ("Parse error: Remaining Tokens " ++  (intercalate ";" (map show tokens)))

}
