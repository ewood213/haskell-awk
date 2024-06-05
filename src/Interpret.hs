module Interpret where
import Parser
import Lexer
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

data Value = ValString ByteString | ValInt Int deriving (Show, Eq, Ord)

interpret :: Expression -> ByteString -> ByteString
interpret (Expression pattern action) bs = do
    if buildPattern pattern bs
    then buildPrintAction action bs
    else BS.empty

buildPattern :: Pattern -> ByteString -> Bool
buildPattern (Pattern v1 binop v2) bs = evalVar v1 bs `op` evalVar v2 bs
    where op = binOpToFn binop
buildPattern Empty _ = True

binOpToFn :: BinaryOp -> (Value -> Value -> Bool)
binOpToFn Eq = (==)
binOpToFn Ne = (/=)
binOpToFn Lt = (<)
binOpToFn Le = (<=)
binOpToFn Gt = (>)
binOpToFn Ge = (>=)

buildPrintAction :: PrintAction -> ByteString -> ByteString
buildPrintAction (PrintAction []) bs = bs
buildPrintAction (PrintAction cs) bs = BS.unwords $ map (valueToString . flip evalVar bs) cs

evalVar :: Var -> ByteString -> Value
evalVar (StringVar s) _ = ValString s
evalVar (IntVar i) _ = ValInt i
evalVar (Colvar 0) s = ValString s
evalVar (Colvar c) s =
    case BS.readInt colvarVal of
        Just (i, "") -> ValInt i
        otherwise -> ValString colvarVal
    where
        colvarVal = BS.words s !! (c - 1)

valueToString :: Value -> ByteString
valueToString (ValString s) = s
valueToString (ValInt i) = BS.pack $ show i

