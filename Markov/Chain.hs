module Markov.Chain where

import qualified Data.Map as M
import System.Random (randomRIO)
import Data.List (sortBy)

-- Markov Chain: DAG with probabilities on arcs
-- also, a dictionary that associates a
-- probability to each successor
type Chain a = M.Map a [(a, Int)]

-- build a Markov chain from a key-value list
fromList :: Ord a => [(a, [(a, Int)])] -> Chain a
fromList = M.fromList

states :: Chain a -> [a]
states = M.keys

-- get the possible state-probability pairs from
-- of a given state
accessibleStates :: Ord a => Chain a -> a -> [(a, Int)]
accessibleStates c s = case M.lookup s c of
    Just accs -> accs
    Nothing -> []

-- sort a list of state-probability pairs by probabilities
sortByProb :: [(a, Int)] -> [(a, Int)]
sortByProb = sortBy (.<.)
    where
    sp .<. sp' = snd sp' `compare` snd sp

-- get the sum probability of a list of accessible states
sumProb :: [(a, Int)] -> Int
sumProb = sum . map snd

-- given a state, randomly select a next state from a chain
-- based on the probability distribution of the accessible
-- states
next :: Ord a => Chain a -> a -> IO (Maybe a)
next c s = do
    let accs = accessibleStates c s
    rn <- randomRIO (0, sumProb accs)
    return $ next' rn 0 $ sortByProb accs
    where
    next' _ _ [] = Nothing
    next' _ _ (sp : []) = Just $ fst sp
    next' rn acc ((s, p) : sps) = -- sample
        if rn <= acc + p
            then Just s
            else next' rn (acc + p) sps

-- take a random walk of a maximum given number of steps
-- through a given Markov process
-- TODO: refactor ugly code
randomWalk :: Ord a => Chain a -> a -> Int -> IO [a]
randomWalk c s n = if n <= 0
    then return []
    else do
        mns <- next c s
        case mns of
            Nothing -> return []
            Just ns -> do
                rw' <- randomWalk c ns (n - 1)
                return $ s : rw'
