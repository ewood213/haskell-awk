{
-- At the top of the file, we define the module and its imports, similarly to Haskell.
module Lexer where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Function
}
-- In the middle, we insert our definitions for the lexer, which will generate the lexemes for our grammar.
%wrapper "monad-bytestring"

$digit = [0-9]

@colvar = \$$digit+

tokens :-

<0> $white+ ;
<0> @colvar { tokColvar }
<0> $digit+ { tokDigit }
<0> print   { \_ _-> pure TokenPrint }
<0> ==      { \_ _ -> pure TokenEq }
<0> \{      { \_ _ -> pure TokenLBrace }
<0> \}      { \_ _ -> pure TokenRBrace }
<0> \,      { \_ _ -> pure TokenComma }
<0> \<      { \_ _ -> pure TokenLt }
<0> \>      { \_ _ -> pure TokenGt }
<0> "!="    { \_ _ -> pure TokenNe }
<0> ">="    { \_ _ -> pure TokenGe }
<0> "<="    { \_ _ -> pure TokenLe }

{

data Token = TokenColvar Int -- This will be changed to bytestring eventually to support variable names
           | TokenEq
           | TokenLBrace
           | TokenRBrace
           | TokenPrint
           | TokenDigit Int
           | TokenEOF
           | TokenComma
           | TokenLt
           | TokenGt
           | TokenNe
           | TokenGe
           | TokenLe
           deriving (Eq, Show)

alexEOF :: Alex Token
alexEOF = pure TokenEOF

tokColvar :: AlexAction Token
tokColvar (_, _, str, _) len =
    BS.tail str
    & BS.take len
    & readDigit
    & TokenColvar
    & pure

tokDigit :: AlexAction Token
tokDigit (_, _, str, _) len =
    str
    & BS.take len
    & readDigit
    & TokenDigit
    & pure

readDigit :: ByteString -> Int
readDigit bs = case BS.readInt bs of
    Just (n, _) -> n
    otherwise -> error "readDigit: not a digit"
}
