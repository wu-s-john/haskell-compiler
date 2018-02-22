{
module Parser.Parser where
import Data.Char

import Data.Maybe
import Data.List

import qualified Lexer.Token as T
import Parser.AST
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
      '<'             { T.LessThanOperator {} }
      '<='            { T.LessThanOrEqualOperator {} }
      '='             { T.IsEqualsOperator {} }
      'new'           { T.NewKeyword {} }
      '('             { T.LeftParenthesesOperator {} }
      ')'             { T.RightParenthesesOperator {} }
      '{'             { T.LeftCurlyBracesOperator {} }
      '}'             { T.RightCurlyBracesOperator {} }
      ':'             { T.ColonOperator {} }
      '<-'            { T.AssignmentOperator {} }
      ';'             { T.SemicolonOperator {} }
      '~'             { T.TildeOperator {} }
      ','             { T.CommaOperator {} }
      'isvoid'        { T.IsvoidKeyword {}}
      'class'         { T.ClassKeyword {} }
      'inherits'      { T.InheritsKeyword {} }
      'not'           { T.NotKeyword {} }
      'let'           { T.LetKeyword {} }
      'in'            { T.InKeyword {} }
      'case'          { T.CaseKeyword {} }
      'of'            { T.OfKeyword {} }
      '=>'            { T.TypeBoundOperator {} }
      'esac'          { T.EsacKeyword {} }
      objectID        { T.ObjectIdentifier {} }
      typeID          { T.TypeIdentifier {} }
      string          { T.StringLiteral {} }

%right expr
%left '~'
%left 'isvoid'
%left '+' '-'
%left '*' '/'
%left '<=' '<' '='
%left 'not'
%right  '<-'
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
        objectID ':' typeID opt_expr     { Attribute (Identifier (T.getName $1)) (Type (T.getName $3)) $4 }

opt_expr :: {Maybe Expression}
opt_expr : {- empty -}            { Nothing }
         | '<-' expr              { Just $2 }

exprs :: { [Expression] }
exprs : expr ';'                  { [$1] }
      | exprs expr ';'            { $1 ++ [$2] }

letBinding :: { LetBinding }
letBinding :
             objectID ':' typeID ',' letBinding           { LetDeclaration (Identifier (T.getName $1)) (Type (T.getName $3)) Nothing $5 }
           | objectID ':' typeID '<-' expr ',' letBinding { LetDeclaration (Identifier (T.getName $1)) (Type (T.getName $3)) (Just $5) $7 }
           | objectID ':' typeID 'in' expr                { LetBinding (Identifier (T.getName $1)) (Type (T.getName $3)) Nothing $5 }
           | objectID ':' typeID '<-' expr 'in' expr      { LetBinding (Identifier (T.getName $1)) (Type (T.getName $3)) (Just $5) $7 }

caseBranch :: { CaseBranch }
caseBranch :
            objectID ':' typeID  '=>' expr ';' {CaseBranch (Identifier (T.getName $1)) (Type (T.getName $3)) $5 }

caseBranches :: { [CaseBranch] }
caseBranches :
                caseBranch               { [ $1 ]}
              | caseBranches caseBranch  { $1 ++ [$2] }

expr :: { Expression }
expr  :
        expr '+' expr           { BinaryOp PlusTerminal $1 $3 }
      | expr '-' expr           { BinaryOp MinusTerminal $1 $3 }
      | expr '*' expr           { BinaryOp TimesTerminal $1 $3 }
      | expr '/' expr           { BinaryOp DivideTerminal $1 $3 }

      | expr '<' expr           { BinaryOp LessThanTerminal $1 $3 }
      | expr '<=' expr          { BinaryOp LessThanOrEqualTerminal $1 $3 }
      | expr '=' expr           { BinaryOp EqualTerminal $1 $3 }
      | expr '<-' expr          { AssignmentExpression $1 $3 }
      | 'new' typeID            { NewExpression (Type (T.getName $2)) }
      | '~' expr                { UnaryOp TildeTerminal $2 }
      | 'isvoid' expr           { UnaryOp IsvoidTerminal $2 }
      | 'not' expr              { UnaryOp NotTerminal $2 }
      | int                     { IntegerExpr (T.getValue $1) }
      | objectID                { IdentifierExpr (T.getName $1) }
      | '(' expr ')'            { $2 }
      | '{' exprs '}'           { BlockExpression $2 }
      | 'let' letBinding        { LetExpression $2 }
      | 'case' expr 'of' caseBranches 'esac' { TypeCaseExpression $2 $4 }
      | string                  { StringExpr (T.getStrVal $1)}

{
parseError :: [T.Token] -> a
parseError tokens = error ("Parse error: Remaining Tokens " ++  (intercalate ";" (map show tokens)))

}
