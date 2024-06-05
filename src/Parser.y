{
module Parser where

import Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
}

%monad { Alex } { >>= } { return }
%lexer { lexer } { TokenEOF }
%name calc
%tokentype { Token }
%error { parseError }

%token
      colvar          { TokenColvar $$ }
      digit           { TokenDigit $$ }
      print           { TokenPrint }
      '=='            { TokenEq }
      '<'             { TokenLt }
      '>'             { TokenGt }
      '{'             { TokenLBrace }
      '}'             { TokenRBrace }
      ','             { TokenComma }
      '!='            { TokenNe }
      '>='            { TokenGe }
      '<='            { TokenLe }

%%

Exps : SingleExp Exps                { $1 : $2 }
      |                              { [] }

SingleExp   : pattern '{' action '}' { Exp1 $1 $3 }
      | '{' action '}'               { Exp1 Empty $2 }
pattern
      : valtoken binop valtoken     { Pattern $1 $2 $3 }

valtoken : colvar                { TokenColvar $1 }
           | digit               { TokenDigit $1 }

binop : '==' { Eq }
      | '<'  { Lt }
      | '>'  { Gt }
      | '!=' { Ne }
      | '>=' { Ge }
      | '<=' { Le }

action
      : print colvarList          { Action $2 }
      | print                     { Action [] }

colvarList
      : colvar ',' colvarList     { TokenColvar $1 : $3 }
      | colvar                    { [TokenColvar $1] }


{
data Exp1
      = Exp1 Pattern Action
      deriving Show

data Pattern = Pattern Token BinaryOp Token | Empty deriving Show
data Action = Action [Token] deriving Show

data BinaryOp = Eq | Lt | Gt | Le | Ge | Ne deriving Show

parseError :: Token -> Alex a
parseError _ = do
  ((AlexPn _ line column), _, _, _) <- alexGetInput
  alexError ("parse error at line " ++ (show line) ++ ", column " ++ (show column))

lexer :: (Token -> Alex a) -> Alex a
lexer = (alexMonadScan >>=)

parse :: ByteString -> Either String [Exp1]
parse s = runAlex s calc
}
