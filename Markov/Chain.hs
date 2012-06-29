module Markov.Chain where

import Data.Map as M

-- Markov Chain: DAG with probabilities on arcs
-- also, a dictionary that associates a
-- probability to each successor
type Chain a = M.Map a [(a, Float)]

-- build a Markov chain from a key-value list
fromList :: Ord a => [(a, [(a, Float)])] -> Chain a
fromList = M.fromList
