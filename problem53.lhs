%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem53
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 18:03:56 CDT 2010
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


There are exactly ten ways of selecting three from five, 12345:

\begin{center}
123, 124, 125, 134, 135, 145, 234, 235, 245, and 345
\end{center}

In combinatorics, we use the notation, $_5C_3 = 10$.

In general,

\begin{equation}
_nC_r = \frac{n!}{r!(n-r_!} 
\end{equation}

where $r \leq n$, $n! = n \times (n¿1) \times \ldots \times 3
\times 2 \times 1$, and $0! = 1$.


It is not until $n = 23$, that a value exceeds one-million: 
$_{23}C_{10} = 1144066$.

How many, not necessarily distinct, values of  $_nC_r$, for 
$1 \leq n \leq 100$, are greater than one-million?



\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import qualified Data.Set as Set
import Data.Numbers
import Data.Numbers.Primes

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n - 1)

(!) :: Int -> Int
(!) 0 = (1 :: Int)
(!) 1 = (1 :: Int)
(!) x = x * (!) (x-1)

choose :: Integer -> Integer -> Integer
choose n r = (factorial n) `div` ((factorial r) * (factorial (n-r)))

permute :: Integer -> Integer -> Integer 
permute n r = (factorial n) `div` (factorial (n-r))


main = do
  let combs = concat $ map (\zz -> map (\z -> choose zz z) [0..zz]) [1..100]
      solns = filter (>=1000000) combs
      nsols = length solns
  putStrLn $ "There are " ++ show nsols ++ " values of nCr which\n"
             ++ "evaluate to more than 1,000,000."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem53.lhs
There are 4075 values of nCr which
evaluate to more than 1,000,000.


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
