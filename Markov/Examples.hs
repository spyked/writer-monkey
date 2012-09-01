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

-- "health" Markov chain
-- age dependency?
data Human = Healthy | Sick | Dead deriving (Eq, Ord, Show)

humanChain :: Chain Human
humanChain = fromList [
    (Healthy, [(Healthy, 0.29), (Sick, 0.7), (Dead, 0.01)]),
    (Sick, [(Healthy, 0.8), (Sick, 0.1), (Dead, 0.1)]),
    (Dead, [(Healthy, 0), (Sick, 0), (Dead, 1)])
    ]

-- character succession example - quite abstract
charChain :: Chain Char
charChain = fromList [
    ('a', [('a', 0.25), ('b', 0.25), ('c', 0.25), ('d', 0.25)]),
    ('b', [('a', 0.2), ('b', 0), ('c', 0.8), ('d', 0)]),
    ('c', [('a', 0.4), ('b', 0.4), ('c', 0.2), ('d', 0)]),
    ('d', [('a', 0.1), ('b', 0.1), ('c', 0.1), ('d', 0.7)])
    ]

-- word succession chain
wordChain :: Chain String
wordChain = fromList [
    ("Ana", [("Ana", 0), ("Ion", 0), ("are", 0.6), ("mananca", 0.3), ("mere", 0.1)]),
    ("Ion", [("Ana", 0), ("Ion", 0), ("are", 0.3), ("mananca", 0.68), ("mere", 0.02)]),
    ("are", [("Ana", 0.01), ("Ion", 0.01), ("are", 0), ("mananca", 0), ("mere", 0.98)]),
    ("mananca", [("Ana", 0.05), ("Ion", 0.05), ("are", 0), ("mananca", 0), ("mere", 0.9)]),
    ("mere", [("Ana", 0.2), ("Ion", 0.2), ("are", 0.25), ("mananca", 0.25), ("mere", 0.1)])
    ]
