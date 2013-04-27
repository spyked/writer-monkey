module Monkey.Util where

import Monkey.Util.Romanian
import Monkey.Util.Processing
import qualified Data.Char as C (toLower)

-- various utility functions
punctuation :: String
punctuation = ",/;'\\[]<>?:\"|{}`~!@#$%^&*()-_=+"

notContains :: Eq a => [a] -> a -> Bool
notContains = flip notElem

stripPunctuation :: String -> String
stripPunctuation = filter $ notContains punctuation

wsPunctuation :: String -> String
wsPunctuation = map $ \ c -> if c `elem` punctuation then ' ' else c

toLower :: String -> String
toLower = map C.toLower

-- return rest as second element of pair, discard period
takeSentence :: String -> (String, String)
takeSentence s = (takeWhile (/= '.') s, tryTail $ dropWhile (/= '.') s)
    where
    tryTail [] = []
    tryTail ('.' : rest) = rest
    tryTail _ = error "Shouldn't get here"

sentences :: String -> [String]
sentences ss = case takeSentence ss of
    (s', []) -> [s']
    (s', ss') -> s' : sentences ss'
