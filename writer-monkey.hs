module Main where

import Markov.Chain
import Monkey.Analyzer
import Monkey.Util
import Monkey.Util.Romanian
import System.Environment (getArgs)
import System.IO (readFile)
import System.Random (randomIO)

preprocess :: String -> [String]
preprocess = words . toLower . wsPunctuation . makeAsciiRo

-- TODO: make it possible to read from stdin
main :: IO ()
main = do
    -- parse args
    args <- getArgs
    let numWords = read $ args !! 0 :: Int
        input = args !! 1
    -- get preprocessed text
    text <- fmap preprocess $ readFile input
    -- generate a random initial state
    r <- fmap (`mod` length text) randomIO
    -- analyze and run random walk
    result <- randomWalk (analyze text) (text !! r) numWords
    putStrLn $ unwords result
