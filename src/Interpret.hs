module Interpret where
import Parser
import Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS


interpret :: Exp1 -> ByteString -> ByteString
interpret (Exp1 pattern action) bs = do
    if buildPattern pattern bs
    then buildAction action bs
    else BS.empty

buildPattern :: Pattern -> ByteString -> Bool
buildPattern (Pattern t1 binop t2) bs = evalToken t1 bs `op` evalToken t2 bs
    where op = binOpToFn binop
buildPattern Empty _ = True

evalToken :: Token -> ByteString -> Int
evalToken (TokenDigit d) _ = d
evalToken tok@(TokenColvar _) s = evalColvarInt tok s

binOpToFn :: BinaryOp -> (Int -> Int -> Bool)
binOpToFn Eq = (==)
binOpToFn Ne = (/=)
binOpToFn Lt = (<)
binOpToFn Le = (<=)
binOpToFn Gt = (>)
binOpToFn Ge = (>=)

buildAction :: Action -> ByteString -> ByteString
buildAction (Action []) bs = bs
buildAction (Action cs) bs = BS.unwords $ map (flip evalColvar bs) cs

evalColvar :: Token -> ByteString -> ByteString
evalColvar (TokenColvar 0) s = s
evalColvar (TokenColvar c) s = BS.words s !! (c - 1)

evalColvarInt :: Token -> ByteString -> Int
evalColvarInt (TokenColvar c) s = read $ BS.unpack $ BS.words s !! (c - 1)
