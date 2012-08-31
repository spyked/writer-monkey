module Monkey.Util where

-- various utility functions
punctuation :: String
punctuation = ",.;?!'-[](){}"

notContains :: Eq a => [a] -> a -> Bool
notContains = flip notElem

stripPunctuation :: String -> String
stripPunctuation = filter $ notContains punctuation
