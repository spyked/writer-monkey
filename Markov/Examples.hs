module Markov.Examples where

import Markov.Chain

-- possible states of the weather
data Weather = Sunny | Rainy deriving (Eq, Ord, Show)

-- a "Weather Markov chain"
weatherChain :: Chain Weather
weatherChain = fromList [
    (Sunny, [(Sunny, 6), (Rainy, 4)]),
    (Rainy, [(Sunny, 7), (Rainy, 3)])
    ]

-- "drunkard" random walk
data Direction = L | R deriving (Eq, Ord, Show)

drunkardChain :: Chain Direction
drunkardChain = fromList [
    (L, [(L, 1), (R, 1)]),
    (R, [(L, 1), (R, 1)])
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
    (Healthy, [(Healthy, 29), (Sick, 70), (Dead, 1)]),
    (Sick, [(Healthy, 80), (Sick, 10), (Dead, 10)]),
    (Dead, [(Healthy, 0), (Sick, 0), (Dead, 100)])
    ]

-- character succession example - quite abstract
charChain :: Chain Char
charChain = fromList [
    ('a', [('a', 25), ('b', 25), ('c', 25), ('d', 25)]),
    ('b', [('a', 20), ('b', 0), ('c', 80), ('d', 0)]),
    ('c', [('a', 40), ('b', 40), ('c', 20), ('d', 0)]),
    ('d', [('a', 10), ('b', 10), ('c', 10), ('d', 70)])
    ]

-- word succession chain
wordChain :: Chain String
wordChain = fromList [
    ("Ana", [("Ana", 0), ("Ion", 0), ("are", 60), ("mananca", 30), ("mere", 10)]),
    ("Ion", [("Ana", 0), ("Ion", 0), ("are", 30), ("mananca", 68), ("mere", 2)]),
    ("are", [("Ana", 1), ("Ion", 1), ("are", 0), ("mananca", 0), ("mere", 98)]),
    ("mananca", [("Ana", 5), ("Ion", 5), ("are", 0), ("mananca", 0), ("mere", 90)]),
    ("mere", [("Ana", 20), ("Ion", 20), ("are", 25), ("mananca", 25), ("mere", 10)])
    ]
