module Monkey.Analyzer (analyze) where

-- text analysis resulting in a Markov model

import Monkey.Util.Processing
import Markov.Chain
import Data.Monoid (mappend)

-- we assume that a "text" is actually a sequence of tokens
analyze :: (Eq a, Ord a) => [a] -> Chain a
analyze = foldl mappend emptyChain
    . map (uncurry sampleConsecutive)
    . groupConsecutives

-- given two consecutive tokens, generate a chain
sampleConsecutive :: a -> a -> Chain a
sampleConsecutive t1 t2 = singletonChain t1 $ singletonDistrib t2

{-
analyze' text = fromList $ zip uniqs $ map doAnalyze uniqs
    where
    uniqs = nub text
    doAnalyze = flip analyzeToken $ text

-- given a token and its context, construct a follower-probability
-- model
analyzeToken :: Eq a => a -> [a] -> [(a, Int)]
analyzeToken x = occurenceList . consecutivesOf x
-}
