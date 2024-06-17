{
module Parser where

import Lexer
import AST
import qualified Value
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%monad { Alex } { >>= } { return }
%lexer { lexer } { TokenEOF }
%name calc
%tokentype { Token }
%error { parseError }

%token
      float           { TokenFloat $$ }
      varname         { TokenVarName $$ }
      string          { TokenString $$ }
      print           { TokenPrint }
      '+'             { TokenPlus }
      '-'             { TokenMinus }
      '*'             { TokenTimes }
      '%'             { TokenMod }
      '/'             { TokenDivide }
      '('             { TokenLParen }
      ')'             { TokenRParen }
      '&&'            { TokenAnd }
      '||'            { TokenOr }
      '=='            { TokenEquality }
      '!='            { TokenInequality }
      '!'             { TokenNot }
      '<'             { TokenLessThan }
      '<='            { TokenLessEqual }
      '>'             { TokenGreaterThan }
      '>='            { TokenGreaterEqual }
      '='             { TokenAssign }
      '+='            { TokenPlusEquals }
      '-='            { TokenMinusEquals }
      '*='            { TokenTimesEquals }
      '/='            { TokenDivideEquals }
      '%='            { TokenModEquals }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }
      ','             { TokenComma }
      ';'             { TokenSemiColon }
      '$'             { TokenDollar }
      NR              { TokenNR }
      NF              { TokenNF }
      begin           { TokenBegin }
      end             { TokenEnd }

%%
awkprogram
    : awkstatementlist      { $1 }
	| expression            { [AwkStatement $1 [PrintAction []]] }

awkstatementlist
    : awkstatement awkstatementlist { $1 : $2}
    |                               { [] }
awkstatement
	: expression '{' actionlist '}' { AwkStatement $1 $3 }
    | begin '{' actionlist '}'      { AwkStatement BeginExpression $3 }
    | end '{' actionlist '}'        { AwkStatement EndExpression $3 }
	| '{' actionlist '}'            { AwkStatement (FloatExpression 1) $2 }

actionlist
	: action ';' actionlist	{ $1 : $3 }
	| action                { [$1] }

action
	: assignaction     { $1 }
	| print explist	   { PrintAction $2 }
	| print		       { PrintAction [] }

explist
	: expression ',' explist { $1 : $3 }
	| expression		     { [$1] }

assignaction
        : varname '='  expression    { AssignAction $1 $3 }
        | varname '+=' expression    { AssignUpdateAction $1 Value.plus $3 }
        | varname '-=' expression    { AssignUpdateAction $1 Value.minus $3 }
        | varname '*=' expression    { AssignUpdateAction $1 Value.times $3 }
        | varname '/=' expression    { AssignUpdateAction $1 Value.divide $3 }
        | varname '%=' expression    { AssignUpdateAction $1 Value.mod $3 }

expression
        : expression '||' andexp  { BinaryExpression $1 Value.or $3 }
        | andexp             { $1 }

andexp
        : andexp '&&' equalityexp { BinaryExpression $1 Value.and $3 }
        | equalityexp             { $1 }

equalityexp
        : equalityexp '==' orderexp { BinaryExpression $1 Value.equal $3 }
        | equalityexp '!=' orderexp { BinaryExpression $1 Value.nequal $3}
        | orderexp                  { $1 }

orderexp
        : orderexp '<' addexp       { BinaryExpression $1 Value.lessThan $3 }
        | orderexp '<=' addexp      { BinaryExpression $1 Value.lessEqual $3 }
        | orderexp '>' addexp       { BinaryExpression $1 Value.greaterThan $3 }
        | orderexp '>=' addexp      { BinaryExpression $1 Value.greaterEqual $3 }
        | addexp                    { $1 }

addexp
        : addexp '+' mulexp       { BinaryExpression $1 Value.plus $3 }
        | addexp '-' mulexp       { BinaryExpression $1 Value.minus $3 }
        | mulexp                  { $1 }

mulexp
        : mulexp '*' primary          { BinaryExpression $1 Value.times $3 }
        | mulexp '/' primary          { BinaryExpression $1 Value.divide $3 }
        | mulexp '%' primary          { BinaryExpression $1 Value.mod $3 }
        | primary                     { $1 }

primary
        : '(' expression ')'          { $2 }
        | '-' primary                 { UnaryExpression $2 Value.negate }
        | '!' primary                 { UnaryExpression $2 Value.not }
        | float                       { FloatExpression $1 }
        | varname                     { VarNameExpression $1 }
        | string                      { StringExpression $1 }
        | '$' primary                 { ColvarExpression $2 }
        | NR                          { NRExpression }
        | NF                          { NFExpression }

{
parseError :: Token -> Alex a
parseError _ = do
  ((AlexPn _ line column), _, _, _) <- alexGetInput
  alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

parse :: ByteString -> Either String [AwkStatement]
parse s = runAlex s calc
}
