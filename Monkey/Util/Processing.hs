module Monkey.Util.Processing where

import Data.List (nub, (\\))

-- use these to process lists of stuff
countOccurences :: Eq a => a -> [a] -> Int
countOccurences x = length . filter (== x)

occurenceList :: Eq a => [a] -> [(a, Int)]
occurenceList xs = zip uniqs $ map (flip countOccurences $ xs) uniqs
    where
    uniqs = nub xs

-- gets all the elements that follow a given element
-- in (or not in) the list
consecutivesOf :: Eq a => a -> [a] -> [a]
consecutivesOf x (x' : x'' : xs)
    | x == x' = x'' : consecutivesOf x (x'' : xs)
    | otherwise = consecutivesOf x (x'' : xs)
consecutivesOf _ _ = []

-- assumes elements of second argument are unique
-- this shouldn't be needed, as states with zero
-- probability are theoretically unreachable.
{-
addNonConsecutives :: Eq a => [(a, Int)] -> [a] -> [(a, Int)]
addNonConsecutives fs all = fs ++ map addZero (all \\ fsts)
    where
    fsts = map fst fs
    addZero x = (x, 0)
-}
