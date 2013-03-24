module Markov.Chain where

import qualified Data.Map as M
import Data.List (sortBy)
import Control.Arrow (second)

-- Markov Chain: DAG with probabilities on arcs
-- also, a dictionary that associates a
-- probability to each successor
type Distribution a = M.Map a Int
newtype Chain a = Chain { unChain :: M.Map a (Distribution a) }
                deriving (Eq, Ord, Show)

-- build a Markov chain from a key-value list
fromList :: Ord a => [(a, [(a, Int)])] -> Chain a
fromList = Chain . M.fromList . map (second M.fromList)

states :: Chain a -> [a]
states = M.keys . unChain

distributions :: Chain a -> [Distribution a]
distributions = M.elems . unChain

-- get the possible state-probability pairs from
-- of a given state
accessibleStates :: Ord a => Chain a -> a -> [(a, Int)]
accessibleStates c s = case M.lookup s (unChain c) of
    Just accs -> M.toList accs -- TODO: use Distribution
    Nothing -> []

-- sort a list of state-probability pairs by probabilities
sortByProb :: [(a, Int)] -> [(a, Int)]
sortByProb = sortBy (.<.)
    where
    sp .<. sp' = snd sp' `compare` snd sp

-- get the sum probability of a list of accessible states
sumProb :: [(a, Int)] -> Int
sumProb = sum . map snd
