module Monkey.Util where

import Monkey.Util.Romanian
import qualified Data.Char as C (toLower)

-- various utility functions
punctuation :: String
punctuation = ",./;\\[]<>?:\"|{}`~!@#$%^&*()-_=+"

notContains :: Eq a => [a] -> a -> Bool
notContains = flip notElem

stripPunctuation :: String -> String
stripPunctuation = filter $ notContains punctuation

wsPunctuation :: String -> String
wsPunctuation = map $ \ c -> if elem c punctuation then ' ' else c

toLower :: String -> String
toLower = map C.toLower
