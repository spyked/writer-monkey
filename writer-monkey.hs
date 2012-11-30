module Main where

import Markov.Chain
import Monkey.Analyzer
import Monkey.Util
import Monkey.Util.Romanian
import Data.Maybe (fromMaybe)
import Control.Exception.Base (assert)
import System.Environment (getArgs)
import System.IO
import System.Random (randomIO)
import System.Console.GetOpt

defaultNumSteps :: Int
defaultNumSteps = 100

-- getopts-style, flag options and functions
data Flag =
      NumSteps String
    | ParSize String
    | Preprocess String
    | Output String
    deriving Eq

options :: [OptDescr Flag]
options = [
    Option ['n'] ["numsteps"] (ReqArg NumSteps "num_steps") n_desc,
    Option ['s'] ["parsize"] (ReqArg ParSize "par_size") s_desc,
    Option ['p'] ["preprocess"] (OptArg preprlang "ro") p_desc,
    Option ['o'] ["output"] (ReqArg Output "file") o_desc
    ]
    where
    n_desc = "number of words to generate"
    s_desc = "paragraph size (number of words)"
    p_desc = "preprocess input text"
    o_desc = "output file"

preprlang :: Maybe String -> Flag
preprlang = Preprocess . fromMaybe ""

parseArgs :: [String] -> IO ([Flag], [String])
parseArgs argv = case getOpt Permute options argv of
    (o,n,[])    -> return (o, n)
    (_,_,errs)  -> ioError $ userError $ concat errs ++ usageInfo header options
    where
    header = "Usage: ic [options] [--] [input_files]"

-- flag parsing and other high-level auxiliary funcs
getNumSteps :: [Flag] -> Int
getNumSteps flags = case filter isNumSteps flags of
    []                  -> defaultNumSteps
    (NumSteps s : _)    -> read s
    where
    isNumSteps (NumSteps _) = True
    isNumSteps _ = False

getParSize :: [Flag] -> Int -> Int
getParSize flags textsize = case filter isParSize flags of
    []              -> textsize
    (ParSize s : _) -> read s
    where
    isParSize (ParSize _) = True
    isParSize _ = False

preprocFunc :: [Flag] -> String -> [String]
preprocFunc flags = case filter isPreprocess flags of
    []                      -> words
    (Preprocess "" : _)     -> words . preprocPunct
    (Preprocess "ro" : _)   -> words . preprocPunct . makeAsciiRo
    _                       -> error errMsg
    where
    preprocPunct = toLower . wsPunctuation
    isPreprocess (Preprocess _) = True
    isPreprocess _ = False
    errMsg = "preprocess: only romanian language is supported"

getOutput :: [Flag] -> IO Handle
getOutput flags = case filter isOutput flags of
    []  -> return stdout
    (Output o : _)  -> openFile o WriteMode
    where
    isOutput (Output _) = True
    isOutput _ = False

-- concatenate all the input files in the given order
catInputs :: [FilePath] -> IO String
catInputs [] = hGetContents stdin
catInputs paths = fmap concat $ mapM catInput paths
    where
    -- catInput path = withFile path ReadMode hGetContents
    -- the above definition fails to read data
    catInput = readFile

-- walk from random state
runWalk :: Chain String -> Int -> IO [String]
runWalk model numSteps = do
    let ss = states model
    -- generate a random initial state
    r <- fmap (`mod` length ss) randomIO
    randomWalk model (ss !! r) numSteps

runWalks :: Chain String -> Int -> Int -> IO [String]
runWalks _ 0 _ = return []
runWalks model numSteps parSize = do
    let ns = min numSteps parSize
    result <- runWalk model ns
    rest <- runWalks model (numSteps - ns) parSize
    return $ unwords result : rest

main :: IO ()
main = do
    -- parse args
    (flags, inputs) <- getArgs >>= parseArgs
    let numSteps = getNumSteps flags
        parSize = getParSize flags numSteps
        preprocess = preprocFunc flags
    -- get preprocessed text
    text <- fmap preprocess $ catInputs inputs
    assert (length text /= 0) $ do
    -- analyze and run random walks
    result <- runWalks (analyze text) numSteps parSize
    out <- getOutput flags
    -- put paragraphs
    mapM_ (\ x -> hPutStr out x >> hPutStr out "\n\n") result
    hClose out
