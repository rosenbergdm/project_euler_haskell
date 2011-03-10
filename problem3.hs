-----------------------------------------------------------------------------
-- Module       : problem1
-- Copyright    : (c) 2010 David M. Rosenberg
-- License      : BSD3
-- 
-- Maintainer   : David Rosenberg <rosenbergdm@uchicago.edu>
-- Stability    : experimental
-- Portability  : portable
-- Created      : 03/16/10
-- Last modified: 2010 Mar 16
-- 
-- Description  :
--    DESCRIPTION HERE.
-----------------------------------------------------------------------------

----------------------------------------------------------------------------
--                                                                        --
-- The prime factors of 13195 are 5, 7, 13 and 29.                        --
--                                                                        --
-- What is the largest prime factor of the number 600851475143 ?          --
--                                                                        --
----------------------------------------------------------------------------

import System.Environment
import Numeric

divisible :: Integer -> Integer -> Bool
divisible x y = x - (x `div` y * y) == 0

nDivisible x y = (not . divisible x) y

filterComposites :: [Integer] -> Integer -> [Integer]
filterComposites xs x = [ x' | x' <- xs, x' `nDivisible` x ]

largestPrime :: Integer -> Integer
largestPrime n = largestPrime' $ filter (\z -> n `divisible` z) [ 2 .. ( round  . sqrt . fromIntegral ) n   ]

largestPrime' :: [Integer] -> Integer
largestPrime' x
  | length x > 1 = largestPrime' (filterComposites (tail x) (head x)) 
  | otherwise    = head x


data FactorPrime = FactorPrime
  { prime        :: Int
  , mplicity     :: Int
  } deriving (Show, Eq, Ord)

data Factorization = Factorization
  { factors      :: [ FactorPrime ]
  } deriving (Show, Eq, Ord)

primeFactorize :: (Integral a) => a -> Factorization
primeFactorize x = runFactorization x [1..x] f' [] []
  where f' = Factorization []

primeFactorize :: (Integral a) => a -> Factorization -> [a] -> [a] -> Factorization
primeFactorize x [] f primes nonprimes = f
primeFactorize x xs f primes nonprimes =
  let y = head xs
      xs' = tail xs'
      primes' | 



primeFactorize x' xs' f' primes' nonprimes'
  where y = head xs
        xs' = tail xs
        primes' | 

divisMultiplicity :: (Integral a) => a -> a -> a
divisMultiplicity x d
  | x `nDivisible` d        = 0
  | otherwise               = 1 + divisMultiplicity (x / d) d



main = do
  args <- getArgs
  let inputValue = read (args !! 0) :: Integer 
  let outputValue = largestPrime inputValue
  putStrLn $ "The largest prime factor of " ++ (show inputValue) ++ " is " ++ (show outputValue) ++ "\n"
