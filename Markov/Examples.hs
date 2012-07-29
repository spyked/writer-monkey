module Markov.Examples where

import Markov.Chain

-- possible states of the weather
data Weather = Sunny | Rainy deriving (Eq, Ord, Show)

-- a "Weather Markov chain"
weatherChain :: Chain Weather
weatherChain = fromList [
    (Sunny, [(Sunny, 0.6), (Rainy, 0.4)]),
    (Rainy, [(Sunny, 0.7), (Rainy, 0.3)])
    ]

-- "drunkard" random walk
data Direction = L | R deriving (Eq, Ord, Show)

drunkardChain :: Chain Direction
drunkardChain = fromList [
    (L, [(L, 0.5), (R, 0.5)]),
    (R, [(L, 0.5), (R, 0.5)])
    ]

count :: [Direction] -> Int
count [] = 0
count (L : rest) = 1 + count rest
count (R : rest) = -1 + count rest
