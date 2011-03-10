%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem47
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 11:26:49 CDT 2010
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


The first two consecutive numbers to have two distinct prime factors are:

\begin{eqnarray*}
14 = 2 \times 7 \\
15 = 3 \times 5
\end{eqnarray*}

The first three consecutive numbers to have three distinct prime factors are:

\begin{eqnarray*}
644 = 2^2 \times 7 \times 23
645 = 3 \times 5 \times 43
646 = 2 \times 17 \times 19.
\end{eqnarray*}

Find the first four consecutive integers to have four distinct primes factors.
What is the first of these numbers?


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

dPrimeFactors n = Set.fromList $ primeFactors n

dPFList n = [(k, dPrimeFactors k) | k <- filter (\z -> (not $ isPrime z)) [1..n]]


nConsec n s = 
  let dpf   = dPFList s
      fltrd = filter (\z -> Set.size (snd z) == n) dpf 
      gps   = [take (fromIntegral n) (drop (fromIntegral k) fltrd) | k <- [0..(length fltrd - n)] ]
      gps2  = filter (\z -> isConsec (map fst z)) gps
      gps3  = filter (\zz -> Set.empty == foldl (\acc z -> Set.intersection acc (snd z)) (snd (head zz)) zz) gps2 
  in gps3



isConsec xs = (sort xs) == [(minimum xs)..(maximum xs)]


main = do
  let soln = (fst . head . head) $ nConsec 4 20000
  putStrLn $ show soln


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem47.lhs
134043

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
