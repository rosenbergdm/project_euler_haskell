-----------------------------------------------------------------------------
-- Module      : problem7
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 03/19/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can  -- 
-- see that the 6^(th) prime is 13.                                        --
--                                                                         --
-- What is the 10001^(st) prime number?                                    --
-----------------------------------------------------------------------------


import Data.List
import Math.Divisible

generatePrimes :: Int -> [Int]
generatePrimes x = sieveEratosFor x [] [2..]

sieveEratosFor :: Int -> [Int] -> [Int] -> [Int]
sieveEratosFor x primes unclass
  | length primes == x                 = primes
  | otherwise                          = sieveEratosFor x (concat [primes, [y']]) (filter (\z -> z `nDivisible` y') unclass)
    where y' = head unclass




