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
--                                                                        --
-- A palindromic number reads the same both ways. The largest palindrome  --
-- made from the product of two 2-digit numbers is 9009 = 91 * 99.        --
--                                                                        --
-- Find the largest palindrome made from the product of two 3-digit       --
-- numbers.                                                               --
--                                                                        --
----------------------------------------------------------------------------

import Math.Divisible
import Data.List

isPalindrome :: (Integral a) => a -> Bool
isPalindrome x = xf == xb
  where xf = show x
        xb = reverse xf

larg = maximum $ filter (\z -> isPalindrome z) [ x * y | x <- [100..999], y <- [100..999] ]
main = do
         let z = larg
         let x = (filter (\k -> z `divisible` k) [900..999]) !! 0
         let y = (filter (\k -> z `divisible` k) [900..999]) !! 1
         putStrLn $ "The largest palindrome formed as the product of two three"
         putStrLn $ "digit numbers is " ++ show z ++ " = " ++ show x ++ " * " ++ show y ++ "\n\n"
