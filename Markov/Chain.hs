module Markov.Chain where

import Data.Map as M
import System.Random (randomRIO)
import Data.List (sortBy)

-- Markov Chain: DAG with probabilities on arcs
-- also, a dictionary that associates a
-- probability to each successor
type Chain a = M.Map a [(a, Float)]

-- build a Markov chain from a key-value list
fromList :: Ord a => [(a, [(a, Float)])] -> Chain a
fromList = M.fromList

states :: Chain a -> [a]
states = M.keys

-- get the possible state-probability pairs from
-- of a given state
accessibleStates :: Ord a => Chain a -> a -> [(a, Float)]
accessibleStates c s = case M.lookup s c of
    Just accs -> accs
    Nothing -> []

-- sort a list of state-probability pairs by probabilities
sortByProb :: [(a, Float)] -> [(a, Float)]
sortByProb = sortBy (.<.)
    where
    sp .<. sp' = snd sp' `compare` snd sp

-- given a state, randomly select a next state from a chain
-- based on the probability distribution of the accessible
-- states
next :: Ord a => Chain a -> a -> IO a
next c s = do
    rn <- randomRIO (0, 1)
    return $ next' rn 0 $ sortByProb $ accessibleStates c s
    where
    next' _ _ [] = error "No states available."
    next' _ _ (sp : []) = fst sp
    next' rn acc ((s, p) : sps) = -- sample
        if rn <= acc + p
            then s
            else next' rn (acc + p) sps

-- take a random walk of a given number of steps
-- through a given Markov process
randomWalk :: Ord a => Chain a -> a -> Int -> IO [a]
randomWalk c s n = if n <= 0
    then return []
    else do
        ns <- next c s
        rw' <- randomWalk c ns (n - 1)
        return $ s : rw'
