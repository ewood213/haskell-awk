module Interpret where
import Parser
import Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS


interpret :: Exp1 -> ByteString -> ByteString
interpret (Exp1 pattern action) bs = do
    if buildPattern pattern bs
    then buildPrintAction action bs
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

buildPrintAction :: PrintAction -> ByteString -> ByteString
buildPrintAction (PrintAction []) bs = bs
buildPrintAction (PrintAction cs) bs = BS.unwords $ map (flip evalVar bs) cs

evalVar :: Var -> ByteString -> ByteString
evalVar (StringVar s) _ = s
evalVar (IntVar i) _ = BS.pack $ show i
evalVar (Colvar 0) s = s
evalVar (Colvar c) s = BS.words s !! (c - 1)

evalColvar :: Token -> ByteString -> ByteString
evalColvar (TokenColvar 0) s = s
evalColvar (TokenColvar c) s = BS.words s !! (c - 1)

evalColvarInt :: Token -> ByteString -> Int
evalColvarInt (TokenColvar c) s = read $ BS.unpack $ BS.words s !! (c - 1)
