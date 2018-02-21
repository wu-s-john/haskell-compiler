{
module Parser.Parser where
import Data.Char
import qualified Lexer.Token as T
import Parser.AST
import Data.Maybe
import Data.List
import Parser.TerminalNode
}


%name classParser class
%name featureParser feat
%name featuresParser feats
%name expressionParser expr
%tokentype { T.Token }
%error { parseError }


%token
      int             { T.IntegerLiteral {} }
      '+'             { T.PlusOperator {} }
      '-'             { T.MinusOperator {} }
      '*'             { T.TimesOperator {} }
      '/'             { T.DivideOperator {} }
      '('             { T.LeftParenthesesOperator {} }
      ')'             { T.RightParenthesesOperator {} }
      '{'             { T.LeftCurlyBracesOperator {} }
      '}'             { T.RightCurlyBracesOperator {} }
      ':'             { T.ColonOperator {} }
      '<-'            { T.AssignmentOperator {} }
      ';'             { T.SemicolonOperator {} }
      'class'         { T.ClassKeyword {} }
      'inherits'      { T.InheritsKeyword {} }
      objectID        { T.ObjectIdentifier {} }
      typeID          { T.TypeIdentifier {} }

%left '+' '-'
%left '*' '/'
%%

class :: { Class }
class :
        'class' typeID '{' feats '}'    { OrphanedClass (Type (T.getName $2)) $4 }
      | 'class' typeID  'inherits' typeID '{' feats '}'    { InheritedClass (Type (T.getName $2)) (Type (T.getName $4)) $6 }

feats :: { [Feature] }
feats :
        {- empty -}               { [] }
      | feats feat ';'     { $1 ++ [$2] }

feat :: { Feature }
feat :
        objectID ':' typeID opt_expr     { Attribute (Identifier (T.getName $1)) (Type (T.getName $3)) $4}

opt_expr :: {Maybe Expression}
opt_expr : {- empty -}            { Nothing }
         | '<-' expr              { Just $2 }

exprs :: { [Expression] }
exprs : expr ';'                  { [$1] }
      | exprs expr ';'            { $1 ++ [$2] }

expr :: { Expression }
expr  :
        expr '+' expr           { BinaryOp PlusTerminal $1 $3 }
      | expr '-' expr           { BinaryOp MinusTerminal $1 $3 }
      | expr '*' expr           { BinaryOp TimesTerminal $1 $3 }
      | expr '/' expr           { BinaryOp DivideTerminal $1 $3 }
      | int                     { IntegerExpr (T.getValue $1) }
      | objectID                { IdentifierExpr (T.getName $1) }
      | '(' expr ')'            { $2 }
      | '{' exprs '}'           { BlockExpression $2 }

{
parseError :: [T.Token] -> a
parseError tokens = error ("Parse error: Remaining Tokens " ++  (intercalate ";" (map show tokens)))

}
