%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem58
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sat Apr  3 14:14:42 CDT 2010
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

Starting with 1 and spiralling anticlockwise in the following way, a square spiral with side length 7 is formed.

\begin{center}
\begin{tabular}{rrrrrrr}
{\color{red}37} & 36 & 35 & 34 & 33 & 32 & {\color{red}31} \\
38 & {\color{red}17} & 16 & 15 & 14 & {\color{red}13} & 30 \\
39 & 18 & {\color{red}5} & 4 & {\color{red}3} & 12 & 29 \\
40 & 19 & 6 & 1 & 2 & 11 & 28 \\
41 & 20 & {\color{red}7} & 8 & 9 & 10 & 27 \\
42 & 21 & 22 & 23 & 24 & 25 & 26 \\
{\color{red}43} & 44 & 45 & 46 & 47 & 48 & 49 \\
\end{tabular}
\end{center}

It is interesting to note that the odd squares lie along the bottom right
diagonal, but what is more interesting is that 8 out of the 13 numbers lying
along both diagonals are prime; that is, a ratio of $8/13 \approx 62\%$.

If one complete new layer is wrapped around the spiral above, a square spiral
with side length 9 will be formed. If this process is continued, what is the
side length of the square spiral for which the ratio of primes along both
diagonals first falls below $10\%$?


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes
import qualified Data.Set as Set

data Layer = Layer 
  { layerNo :: Int
  , allVals :: [Int]
  , corVals :: [Int]
  , nPrimes :: Int
  , nTotal  :: Int
  } deriving (Show, Ord, Eq, Read)

cornerVals :: Layer -> [Int]
cornerVals lay = 
  let fstCorner = (allVals lay) !! 0 - 1
      interval  = (filter odd [0..]) !! (layerNo lay - 1) - 1
      lstCorner = maximum $ allVals lay
      corners  = if layerNo lay == 1
                    then [1]
                    else tail $ [fstCorner, fstCorner + interval .. lstCorner]
  in corners

nextLayer :: [Layer] -> [Layer]
nextLayer [] = [layer1]
  where layer1 = Layer 1 [1] [1] 0 1
nextLayer ls = concat [ls, [nLayer]]
  where lastLayer = last ls
        lNo       = layerNo lastLayer + 1
        nEntries  = (lNo * 2 - 1)^2 - (sum $ map nTotal ls)
        aVals     = [((maximum $ allVals lastLayer) + 1) .. ((maximum $ allVals lastLayer)+nEntries)]
        corners   = cornerVals (Layer lNo aVals [1] 0 nEntries)
        pCorners  = filter (isPrime . fromIntegral) corners
        nLayer    = Layer lNo aVals corners (length pCorners) nEntries


buildNLayers :: Int -> [Layer]
buildNLayers 1 = nextLayer []
buildNLayers n = nextLayer $ buildNLayers (n-1)

--calcCornerPrimeRatio :: [Layer] -> Double
calcCornerPrimeRatio lays = (fromIntegral $ sum $ map nPrimes lays :: Double) / (1.0 + 4.0 * (fromIntegral $ (length lays) - 1))


find2km :: Integral a => a -> (a,a)
find2km n = f 0 n
    where 
        f k m
            | r == 1 = (k,m)
            | otherwise = f (k+1) q
            where (q,r) = quotRem m 2        
 
millerRabinPrimality :: Integer -> Integer -> Bool
millerRabinPrimality n a
    | a <= 1 || a >= n-1 = 
        error $ "millerRabinPrimality: a out of range (" 
              ++ show a ++ " for "++ show n ++ ")" 
    | n < 2 = False
    | even n = False
    | b0 == 1 || b0 == n' = True
    | otherwise = iter (tail b)
    where
        n' = n-1
        (k,m) = find2km n'
        b0 = powMod n a m
        b = take (fromIntegral k) $ iterate (squareMod n) b0
        iter [] = False
        iter (x:xs)
            | x == 1 = False
            | x == n' = True
            | otherwise = iter xs
 
pow' :: (Num a, Integral b) => (a -> a -> a) -> (a -> a) -> a -> b -> a
pow' _ _ _ 0 = 1
pow' mul sq x' n' = f x' n' 1
    where 
        f x n y
            | n == 1 = x `mul` y
            | r == 0 = f x2 q y
            | otherwise = f x2 q (x `mul` y)
            where
                (q,r) = quotRem n 2
                x2 = sq x
 
mulMod :: Integral a => a -> a -> a -> a
mulMod a b c = (b * c) `mod` a
squareMod :: Integral a => a -> a -> a
squareMod a b = (b * b) `rem` a
powMod :: Integral a => a -> a -> a -> a
powMod m = pow' (mulMod m) (squareMod m)

isPrime1 x
    |x==3=True
    |otherwise=and [millerRabinPrimality x n|n<-[2,3]]
diag = 1:3:5:7:zipWith (+) diag [8,10..]
problem_58 = 
    result $ dropWhile tooBig $ drop 2 $ scanl primeRatio (0,0) diag
    where
    primeRatio (n,d) num = (if d `mod` 4 /= 0 && isPrime1 num then n+1 else n,d+1)
    tooBig (n,d) = n*10 >= d
    result ((_,d):_) = (d+2) `div` 4 * 2 + 1



main = do
  args <- getArgs
  putStrLn $ "INCOMPLETE" 


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem58.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
