%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem35
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sun Mar 28 13:25:46 CDT 2010
%% 
%% Description :
%%    Project euler problem solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\documentclass{article}
%include colorcode.fmt
\usepackage{graphicx}
\usepackage{color}
\usepackage{pgf}

\begin{document}

\section{Problem}


The number, $197$, is called a circular prime because all rotations of the
digits: $197, 971, and 719$, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71,
73, 79, and 97.

How many circular primes are there below one million?






\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes
import Control.Monad (replicateM)


smallPrimes = filter (<1000000) $ (take 78498 primes)

getRotations :: Integer -> [Integer]
getRotations x = getRotations' ((length $ show x) - 1) x

getRotations' :: Int -> Integer -> [Integer]
getRotations' 0 x = [x]
getRotations' n x = (x: (getRotations' (n-1) x'))
  where d  = show x
        d' = concat [drop 1 d, [head d]]
        x' = read d' :: Integer

isCircularPrime :: Integer -> Bool
isCircularPrime x = isPrime x && (and $ map isPrime (getRotations x))




intListToInt :: [Integer] -> Integer
intListToInt []     = 0
intListToInt (x:xs) = 10^(length xs) * x + intListToInt xs


intToIntList :: Integer -> [Integer]
intToIntList x
  | x < 10          = [x]
  | otherwise       = concat [(intToIntList $ x - 10 * x `div` 10), [x]]

 
canBeCircularPrimeList = [1,3,7,9]
 
listToInt n = foldl (\x y -> 10*x+y) 0 n
rot n l = y ++ x where (x,y) = splitAt n l
allrots l = map (\x -> rot x l) [0..(length l)-1]
isCircular l =  all (isPrime . listToInt) $ allrots l
circular 1 = [[2],[3],[5],[7]]  -- a slightly special case
circular n = filter isCircular $ replicateM n canBeCircularPrimeList
 
problem_35 = length $ concatMap circular [1..6]



main = do
  let circPrimeList = filter isCircularPrime smallPrimes
      nCircPrimes    = length circPrimeList
  putStrLn $ "There are a total of " ++  show nCircPrimes ++ 
            " circular primes below 1000000."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem35.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
