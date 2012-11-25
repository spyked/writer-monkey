module Monkey.Analyzer (analyze) where

-- text analysis resulting in a Markov model

import Markov.Chain (Chain, fromList)
import Data.List (nub)

-- we assume that a "text" is actually a sequence of words, each word
-- being a token for the analyzer.
analyze :: [String] -> Chain String
analyze = undefined
