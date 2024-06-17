module Runner where
import Interpret
import Parser
import AST
import Data.List (partition)
import Control.Monad.RWS.Lazy
import qualified Value
import qualified Data.Map as Map
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.State
import Control.Monad.Except

moveToEnd :: (a -> Bool) -> [a] -> [a]
moveToEnd predicate xs = nonMatching ++ matching
  where
    (matching, nonMatching) = partition predicate xs

isEndExpression :: AwkStatement -> Bool
isEndExpression (AwkStatement EndExpression _) = True
isEndExpression a = False

runProgram :: [AwkStatement] -> ByteString -> ByteString
runProgram progs dat = result
    where
        dataLines = BS.lines dat
        len = length dataLines
        sortedProgs = moveToEnd isEndExpression progs
        -- Add lines for begin and end expressions
        lineInfos = LineInfo "" 0 False :
                    zipWith (\line lineno -> LineInfo line lineno (lineno==len)) (BS.lines dat) [1..]
        eval = sequence $ [interpretProgram prog line | line <- lineInfos, prog <- sortedProgs]
        (_, result) = evalRWS eval () Map.empty

