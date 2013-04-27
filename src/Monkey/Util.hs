module Monkey.Util where

import Monkey.Util.Romanian
import Monkey.Util.Processing
import qualified Data.Char as C (toLower)

-- various utility functions
punctuation :: String
punctuation = ",/'\\[]<>\"|{}`~!@#$%^&*()-_=+"

endPunctuation :: String
endPunctuation = ".?:;"

notContains :: Eq a => [a] -> a -> Bool
notContains = flip notElem

stripPunctuation :: String -> String
stripPunctuation = filter $ notContains punctuation

wsPunctuation :: String -> String
wsPunctuation = map $ \ c -> if c `elem` punctuation then ' ' else c

toLower :: String -> String
toLower = map C.toLower

endOfSentence :: Char -> Bool
endOfSentence = (`elem` endPunctuation)

-- return rest as second element of pair, discard period
takeSentence :: String -> (String, String)
takeSentence s = (takeWhile (not . endOfSentence) s,
                  tryTail $ dropWhile (not . endOfSentence) s)
    where
    tryTail [] = []
    tryTail (c : rest) = if endOfSentence c
        then rest else error "Shouldn't get here"

sentences :: String -> [String]
sentences ss = case takeSentence ss of
    (s', []) -> [s']
    (s', ss') -> s' : sentences ss'
