module Markov.Chain where

import qualified Data.Map as M
import Data.Monoid
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

fromDistrib :: Ord a => [(a, Distribution a)] -> Chain a
fromDistrib = Chain . M.fromList

distribFromList :: Ord a => [(a, Int)] -> Distribution a
distribFromList = M.fromList

states :: Chain a -> [a]
states = M.keys . unChain

distribs :: Chain a -> [Distribution a]
distribs = M.elems . unChain

emptyDistrib :: Distribution a
emptyDistrib = M.empty

singletonDistrib :: a -> Distribution a
singletonDistrib = flip M.singleton $ 1

emptyChain :: Chain a
emptyChain = Chain M.empty

singletonChain :: a -> Distribution a -> Chain a
singletonChain a = Chain . M.singleton a

distribToAscList :: Distribution a -> [(a, Int)]
distribToAscList = M.toAscList

-- get the possible state-probability pairs from
-- of a given state
distribOf :: Ord a => Chain a -> a -> Distribution a
distribOf c s = case M.lookup s (unChain c) of
    Just accs -> accs
    Nothing -> emptyDistrib

-- add state to a distribution
addState :: Ord a => Distribution a -> a -> Distribution a
addState dist s = M.insertWith (+) s 1 dist

-- append two distributions
appendDistribs :: Ord a => Distribution a -> Distribution a -> Distribution a
appendDistribs = M.unionWith (+)

-- Chains are Monoids
instance Ord a => Monoid (Chain a) where
    mempty = emptyChain
    mappend (Chain m1) (Chain m2) = Chain $ M.unionWith appendDistribs m1 m2
