module Interpret where
import Parser
import Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS


interpret :: Exp1 -> ByteString -> IO ()
interpret (Exp1 pattern action) bs = do
    if buildPattern pattern bs
    then buildAction action bs
    else return ()

buildPattern :: Pattern -> ByteString -> Bool
buildPattern (Pattern c binop (TokenDigit d)) bs = evalColvarInt c bs `op` d
    where op = binOpToFn binop

binOpToFn :: BinaryOp -> (Int -> Int -> Bool)
binOpToFn Eq = (==)
binOpToFn Ne = (/=)
binOpToFn Lt = (<)
binOpToFn Le = (<=)
binOpToFn Gt = (>)
binOpToFn Ge = (>=)

buildAction :: Action -> ByteString -> IO ()
buildAction (Action cs) bs = BS.putStrLn $ BS.unwords $ map (flip evalColvar bs) cs

evalColvar :: Token -> ByteString -> ByteString
evalColvar (TokenColvar c) s = BS.words s !! (c - 1)

evalColvarInt :: Token -> ByteString -> Int
evalColvarInt (TokenColvar c) s = read $ BS.unpack $ BS.words s !! (c - 1)
