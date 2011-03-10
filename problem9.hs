-----------------------------------------------------------------------------
-- Module      : problem9
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
-- A Pythagorean triplet is a set of three natural numbers, a  < b  < c,   -- 
-- for which,                                                              --
--                         a^(2) + b^(2) = c^(2)                           --
--                                                                         --
-- For example, 3^(2) + 4^(2) = 9 + 16 = 25 = 5^(2).                       --
--                                                                         --
-- There exists exactly one Pythagorean triplet for which                  --
-- a + b + c = 1000.                                                       --
-- Find the product abc.                                                   --
-----------------------------------------------------------------------------

data PythagTriple = PythagTriple
  { a :: Int
  , b :: Int
  , c :: Int
  } deriving (Show, Eq, Ord)


isPythagTriple :: PythagTriple -> Bool
isPythagTriple pt = a' ^ 2 + b' ^ 2 - c' ^2 == 0
  where a' = a pt
        b' = b pt
        c' = c pt

findPTriples :: Int -> Int -> [PythagTriple]
findPTriples cc m = filter (\z -> isPythagTriple z) [PythagTriple kk (m - cc - kk) cc | kk <- [1..(m-cc-1)] ]

findAllPTriples = concat [findPTriples k 1000 | k <- [1..999]]


