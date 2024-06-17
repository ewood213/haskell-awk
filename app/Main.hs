{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Runner
import Parser
import qualified Value
import System.Environment
import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map
import Data.Foldable

main :: IO ()
main = do
    [prog, file] <- getArgs
    case parse $ BS.pack prog of
        Left s -> putStrLn s
        Right p -> do
            contents <- BS.readFile file
            BS.putStr $ runProgram p contents
             

