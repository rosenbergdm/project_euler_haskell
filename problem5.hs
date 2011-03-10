-----------------------------------------------------------------------------
-- Module       : problem4
-- Copyright    : (c) 2010 David M. Rosenberg
-- License      : BSD3
-- 
-- Maintainer   : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability    : experimental
-- Portability  : portable
-- Created      : 03/19/10
-- Last modified: 2010 Mar 19
-- 
-- Description  :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- 2520 is the smallest number that can be divided by each of the numbers --
-- from 1 to 10 without any remainder.                                    --
--                                                                        --
-- What is the smallest number that is evenly divisible by all of the     --
-- numbers from 1 to 20?                                                  --
--                                                                        --
--                                                                        --
----------------------------------------------------------------------------

import Math.Divisible
import Data.List
import System.Process


combineFactLists :: [FactorPair] -> [FactorPair] -> [FactorPair]
combineFactLists fa fb = reduceFactList (concat [fa, fb]) []

reduceFactList :: [FactorPair] -> [FactorPair] -> [FactorPair]
reduceFactList [] fl = fl
reduceFactList f fl  = reduceFactList f' fl'
  where x'  = head f
        xp  = prime x'
        m   = maximum $ map (\x -> mult x) (filter (\x -> prime x == xp) f)
        f' = filter (\x -> prime x /= xp) f
        fl'  = concat [fl, [FactorPair xp m]]

isPrime :: (Integral a) => a -> Bool
isPrime x 
  | filter (\z -> x `divisible` z) [1..x] == [1, x] = True
  | otherwise                                       = False

primeFactorList :: Int -> [FactorPair]
primeFactorList x
  | isPrime x   = concat [getFactors x, [FactorPair x 1]]
  | otherwise   = getFactors x

primeFactorsTwenty :: [[FactorPair]]
primeFactorsTwenty = [ primeFactorList k | k <- [2..20] ]

completeList = foldl (\acc x -> combineFactLists acc x) [] primeFactorsTwenty

st = showFactorPairList completeList
val = runCommand $ "echo '" ++ st ++ "' | bc"

