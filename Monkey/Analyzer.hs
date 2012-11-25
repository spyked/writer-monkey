module Monkey.Analyzer (analyze) where

-- text analysis resulting in a Markov model

import Monkey.Util.Processing
import Markov.Chain (Chain, fromList)
import Data.List (nub)

-- we assume that a "text" is actually a sequence of tokens
analyze :: (Eq a, Ord a) => [a] -> Chain a
analyze text = fromList $ zip uniqs $ map doAnalyze uniqs
    where
    uniqs = nub text
    doAnalyze = flip analyzeToken $ text

-- given a token and its context, construct a follower-probability
-- model
analyzeToken :: Eq a => a -> [a] -> [(a, Float)]
analyzeToken x = normalizeFollowers x . occurenceList . consecutivesOf x

-- convert integers into probabilities
-- by default, states without followers loop into themselves (first equation)
normalizeFollowers :: Eq a => a -> [(a, Int)] -> [(a, Float)]
normalizeFollowers x [] = [(x,1)]
normalizeFollowers _ fs = map (./. total) fs
    where
    total = fromIntegral . sum $ map snd fs
    (x, occ) ./. n = (x, fromIntegral occ / n)
