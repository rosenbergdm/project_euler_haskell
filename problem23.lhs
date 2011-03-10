%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem23
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 06:28:47 CDT 2010
%% 
%% Description :
%%    Project euler problem solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\documentclass{article}
%include polycode.fmt
\usepackage{graphicx}
\usepackage{color}
\usepackage{pgf}

\begin{document}

\section{Problem}


A perfect number is a number for which the sum of its proper divisors is
exactly equal to the number. For example, the sum of the proper divisors of 28
would be $1 + 2 + 4 + 7 + 14 = 28$, which means that 28 is a perfect number.

A number n is called deficient if the sum of its proper divisors is less than n
and it is called abundant if this sum exceeds n.

As 12 is the smallest abundant number, $1 + 2 + 3 + 4 + 6 = 16$, the smallest
number that can be written as the sum of two abundant numbers is 24. By
mathematical analysis, it can be shown that all integers greater than 28123 can
be written as the sum of two abundant numbers. However, this upper limit cannot
be reduced any further by analysis even though it is known that the greatest
number that cannot be expressed as the sum of two abundant numbers is less than
this limit.

Find the sum of all the positive integers which cannot be written as the sum of
two abundant numbers.

\section{Solution}

\begin{code}

import Data.List
import Data.Maybe
import qualified Data.Map as Map
import Data.Numbers

data NumCategory = Abundant | Perfect | Deficient
  deriving (Show, Ord, Eq, Enum)

sumPropDivs :: Integer -> Integer
sumPropDivs x = sum $ filter (< x) (factors x)

catNumber :: Integer -> NumCategory
catNumber x 
  | x < spd   = Abundant
  | x == spd  = Perfect
  | otherwise = Deficient
    where spd = sumPropDivs x

abundantList = filter (\z -> catNumber z == Abundant) [1..20161]

canWriteSumAb :: Integer -> Bool
canWriteSumAb x = 
  let al    = filter (< x) abundantList
      oal   = filter odd al
      eal   = filter even al
      pairs = case (x <= 48, odd x) of 
          (True, _)      -> intersect (map (\z -> x - z) al) al
          (False, True)  -> intersect (map (\z -> x - z) oal) eal
          (False, False) -> (intersect (map (\z -> x - z) oal) oal) ++ (intersect (map (\z -> x - z) eal) eal)
  in (length pairs) > 0

-- cantWriteUnder :: Integer -> [Integer]
-- cantWriteUnder x = concat [a', b', c']
--  where a'   = map (not . canWriteSumAb) [1..46]
--        odds = filter odd abundantList 

-- main = do
--   let z = sum $ cantWriteUnder 28123
--   putStrLn $ "The sum of all positive numbers which cannot be written as the sum of two abundant numbers is " ++ show z ++ "."

main = do
  let z = sum $ filter (not . canWriteSumAb) [1..20161]
  putStrLn $ "The sum of all positive numbers which cannot be written as the sum of two abundant numbers is " ++ show z ++ "."

\end{code}

\section{Result}

\begin{verbatim}

__SOLUTION1__

\end{verbatim}

__SOLUTION2__

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
