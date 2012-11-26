module Monkey.Util.Romanian where

-- Utility functions for Romanian language
roDiacriticToAscii :: Char -> Char
roDiacriticToAscii c = case c of
    'ă' -> 'a'
    'â' -> 'a'
    'î' -> 'i'
    'Ă' -> 'A'
    'Â' -> 'A'
    'Î' -> 'I'
    'ș' -> 's'
    'ț' -> 't'
    'Ș' -> 'S'
    'Ț' -> 'T'
    '„' -> ' '
    '”' -> ' '
    _   -> c

makeAsciiRo :: String -> String
makeAsciiRo = map roDiacriticToAscii
