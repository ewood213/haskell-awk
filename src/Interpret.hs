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
buildPattern (Pattern v1 binop v2) bs = evalVar bs v1 `op` evalVar bs v2
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
buildPrintAction (PrintAction cs) bs = BS.unwords $ map (valueToString . evalVar bs) cs

evalVar :: ByteString -> Var -> Value
evalVar _ (StringVar s) = ValString s
evalVar _ (IntVar i) = ValInt i
evalVar s (Colvar c) =
    case BS.readInt colvarVal of
        Just (i, "") -> ValInt i
        otherwise -> ValString colvarVal
    where
        colvarVal = evalColvar c
        w = BS.words s
        evalColvar 0 = s
        evalColvar c = if length w >= c
            then w !! (c - 1)
            else BS.empty

valueToString :: Value -> ByteString
valueToString (ValString s) = s
valueToString (ValInt i) = BS.pack $ show i

