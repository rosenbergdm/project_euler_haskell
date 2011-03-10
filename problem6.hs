-----------------------------------------------------------------------------
-- Module      : problem6
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
-- The sum of the squares of the first ten natural numbers is,             --
-- 1^(2) + 2^(2) + ... + 10^(2) = 385                                      --
--                                                                         --
-- The square of the sum of the first ten natural numbers is,              --
-- (1 + 2 + ... + 10)^(2) = 55^(2) = 3025                                  --
--                                                                         --
-- Hence the difference between the sum of the squares of the first ten    --
-- natural numbers and the square of the sum is 3025 - 385 = 2640.         --
--                                                                         --
-- Find the difference between the sum of the squares of the first one     --
-- hundred natural numbers and the square of the sum.                      --
-----------------------------------------------------------------------------

sumSquare :: Integer -> Integer
sumSquare z = foldl (\acc x -> acc + x^2) 0 [1..z]

squareSum :: Integer -> Integer
squareSum z = (sum [1 .. z]) ^ 2

sumSquareDiff :: Integer -> Integer
sumSquareDiff z = squareSum z - sumSquare z
