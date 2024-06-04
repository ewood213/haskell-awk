import Parser
import Lexer
import Interpret
import System.Environment (getArgs)
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> putStrLn "Please give script file"
        [x] -> putStrLn "Please give input file"
        (x:y:xs) -> awkMain x y
awkMain :: String -> String -> IO ()
awkMain x y = do
    script <- BS.readFile x
    let parsed = parse script
    case parsed of
        Left err -> print err
        Right exp1 -> do
            let lineFn = interpret exp1
            input <- BS.readFile y
            mapM_ lineFn $ BS.lines input