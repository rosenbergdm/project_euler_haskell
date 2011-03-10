%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem37
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Mon Mar 29 09:07:55 CDT 2010
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


The number 3797 has an interesting property. Being prime itself, it is possible
to continuously remove digits from left to right, and remain prime at each
stage: 3797, 797, 97, and 7. Similarly we can work from right to left: 3797,
379, 37, and 3.

Find the sum of the only eleven primes that are both truncatable from left to
right and right to left.

NOTE: 2, 3, 5, and 7 are not considered to be truncatable primes.






\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers.Primes

onePrimes = [1, 2, 3, 5, 7] :: [Int]

buildTruncLeft :: Int -> [Int]
buildTruncLeft x = 
  if isPrime (fromIntegral x)
    then (x:recLefts)
    else []
      where addLefts   = map (\z -> read ((show z) ++ (show x))) [1..9] :: [Int]
            primeLefts = filter (isPrime2 . fromIntegral) addLefts
            recLefts   = concat $ map buildTruncLeft primeLefts


buildTruncRight :: Int -> [Int]
buildTruncRight x   = 
  if isPrime (fromIntegral x)
    then (x:recRights)
    else []
      where addRights   = map (\z -> read ((show x) ++ (show z))) [0..9] :: [Int]
            primeRights = filter (\z -> (isPrime2 . fromIntegral) z &&
                isTruncLeft z && isTruncRight z) addRights
            recRights   = concat $ map buildTruncRight primeRights


isTruncLeft :: Int -> Bool
isTruncLeft x
  | (length $ show x) == 1    = isPrime2 $ fromIntegral x
  | otherwise               = (isPrime2 $ fromIntegral x) && (isTruncLeft x')
    where x' = read $ (tail . show) x :: Int


isTruncRight :: Int -> Bool
isTruncRight x
  | (length $ show x) == 1    = isPrime2 $ fromIntegral x
  | otherwise               = (isPrime2 $ fromIntegral x) && (isTruncRight x')
    where x' = read $ (reverse . tail . reverse . show) x :: Int




buildAllTruncs :: Int -> [Int]
buildAllTruncs x = concat [ [x], recTruncs ]
  where addRights   = map (\z -> read ((show z) ++ (show x))) onePrimes :: [Int]
        primeRecs   = filter isTruncLeft $ filter (isPrime . fromIntegral) addRights
        recTruncs   = concat $ map buildAllTruncs primeRecs 

 
primes2 :: [Integer]
primes2 = 2 : filter ((==1) . length . primeFactors) [3,5..]
 
primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
    where
        factor _ [] = []
        factor m (p:ps) | p*p > m        = [m]
                        | m `mod` p == 0 = p : factor (m `div` p) (p:ps)
                        | otherwise      = factor m ps
 
isPrime2 :: Integer -> Bool
isPrime2 1 = False
isPrime2 n = case (primeFactors n) of
                (_:_:_)   -> False
                _         -> True
 
truncs :: Integer -> [Integer]
truncs n = nub . map read $ (take l . tail . tails) s ++ (take l . tail . inits) s
    where
        l = length s - 1
        s = show n
 
problem_37 = sum $ take 11 [x | x <- dropWhile (<=9) primes, all isPrime2 (truncs x)]


main = do
  args <- getArgs
  putStrLn $ "INCOMPLETE" 


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem37.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
