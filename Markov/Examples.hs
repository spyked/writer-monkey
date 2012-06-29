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
