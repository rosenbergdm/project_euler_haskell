-----------------------------------------------------------------------------
-- Module      : problem10
-- Copyright   : (c) 2010 David M. Rosenberg
-- License     : BSD3
-- 
-- Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability   : experimental
-- Portability : portable
-- Created     : 03/20/10
-- 
-- Description :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

-----------------------------------------------------------------------------
-- The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.                   --
--                                                                         --
-- Find the sum of all the primes below two million                        --
--                                                                         --
-----------------------------------------------------------------------------

import Math.Divisible
import System.Environment

isPrime :: Int -> Bool
isPrime x = filter (\z -> x `divisible` z) [1..x] == [1, x]

allBelow :: Int -> [Int]
allBelow x = sieveOfEratosthenes [] [2..x]

sieveOfEratosthenes :: [Int] -> [Int] -> [Int]
sieveOfEratosthenes primes [] = primes
sieveOfEratosthenes primes xs = sieveOfEratosthenes primes' xs'
  where primes' = concat [primes, [head xs]]
        xs'     = filter (\z -> z `nDivisible` (head xs)) xs

main = do
  args <- getArgs
  let maxPrime = read (args !! 0) :: Int
  let sumPrimes = sum $ allBelow maxPrime
  putStrLn $ "The sum of all primes less than " ++ show maxPrime ++ " is " ++ show sumPrimes ++ ".\n"
