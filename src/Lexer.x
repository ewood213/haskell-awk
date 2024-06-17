{
module Lexer where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function
import Text.Read (readMaybe)
}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monad-bytestring"

@float = [0-9]*([0-9]\.?|\.[0-9])[0-9]*([Ee][\-\+]?[0-9]+)?
@varname = [a-zA-Z_][a-zA-Z_0-9]*
@string = \"(\\.|[^\"])*\"
tokens :-

<0> $white+ ;
<0> \$      { \_ _ -> pure TokenDollar }
<0> print   { \_ _ -> pure TokenPrint }
<0> NR      { \_ _ -> pure TokenNR }
<0> NF      { \_ _ -> pure TokenNF }
<0> BEGIN   { \_ _ -> pure TokenBegin }
<0> END     { \_ _ -> pure TokenEnd }
<0> @float  { tokFloat }
<0> @varname { tokVarName }
<0> @string { tokString }
<0> \{      { \_ _ -> pure TokenLBrace }
<0> \}      { \_ _ -> pure TokenRBrace }
<0> ";"     { \_ _ -> pure TokenSemiColon }
<0> ","     { \_ _ -> pure TokenComma }
<0> "+"     { \_ _ -> pure TokenPlus }
<0> "-"     { \_ _ -> pure TokenMinus }
<0> "*"     { \_ _ -> pure TokenTimes }
<0> "%"     { \_ _ -> pure TokenMod }
<0> "/"     { \_ _ -> pure TokenDivide }
<0> "("     { \_ _ -> pure TokenLParen }
<0> ")"     { \_ _ -> pure TokenRParen }
<0> "&&"    { \_ _ -> pure TokenAnd }
<0> "||"    { \_ _ -> pure TokenOr }
<0> "=="    { \_ _ -> pure TokenEquality }
<0> "!="    { \_ _ -> pure TokenInequality }
<0> "!"     { \_ _ -> pure TokenNot }
<0> "<"     { \_ _ -> pure TokenLessThan }
<0> "<="    { \_ _ -> pure TokenLessEqual }
<0> ">"     { \_ _ -> pure TokenGreaterThan }
<0> ">="    { \_ _ -> pure TokenGreaterEqual }
<0> "="     { \_ _ -> pure TokenAssign }
<0> "+="    { \_ _ -> pure TokenPlusEquals }
<0> "-="    { \_ _ -> pure TokenMinusEquals }
<0> "*="    { \_ _ -> pure TokenTimesEquals }
<0> "/="    { \_ _ -> pure TokenDivideEquals }
<0> "%="    { \_ _ -> pure TokenModEquals }

{
data Token = TokenFloat Float
           | TokenEOF
           | TokenPlus
           | TokenMinus
           | TokenTimes
           | TokenDivide
           | TokenMod
           | TokenLParen
           | TokenRParen
           | TokenAnd
           | TokenOr
           | TokenEquality
           | TokenInequality
           | TokenNot
           | TokenLessThan
           | TokenLessEqual
           | TokenGreaterThan
           | TokenGreaterEqual
           | TokenVarName ByteString
           | TokenAssign
           | TokenPlusEquals
           | TokenMinusEquals
           | TokenTimesEquals
           | TokenDivideEquals
           | TokenModEquals
           | TokenString ByteString
           | TokenPrint
           | TokenRBrace
           | TokenLBrace
           | TokenComma
           | TokenSemiColon
           | TokenDollar
           | TokenNR
           | TokenNF
           | TokenBegin
           | TokenEnd
           deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = pure TokenEOF

tokFloat :: AlexAction Token
tokFloat (_, _, str, _) len =
    str
    & BS.take len
    & readFloat
    & TokenFloat
    & pure

readFloat :: ByteString -> Float
readFloat bs = case readMaybe $ BS.unpack bs of
    Just f -> f
    otherwise -> error "readFloat: not a float"

tokVarName :: AlexAction Token
tokVarName (_, _, str, _) len =
    str
    & BS.take len
    & TokenVarName
    & pure

tokString :: AlexAction Token
tokString (_, _, str, _) len =
    BS.take len str
    & BS.unpack
    & read  -- interpret escape sequences
    & fromString -- convert back to ByteString
    & TokenString
    & pure
}
