%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem41
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Tue Mar 30 08:23:27 CDT 2010
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


We shall say that an n-digit number is pandigital if it makes use of all the
digits 1 to $n$ exactly once. For example, 2143 is a 4-digit pandigital and is
also prime.

What is the largest $n$-digit pandigital prime that exists?

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes

rPrimes :: Integer -> [Integer]
rPrimes dg = takeWhile (<(10^dg)) $ filter (>=(10^(dg-1))) primes

isPandigital :: Integer -> Bool
isPandigital x = x' == (take n "123456789")
  where x' = sort $ show x
        n  = length x'

nDigitPanPrimes :: Integer -> [Integer]
nDigitPanPrimes n = 
  let ns  = map concat $ permutations $ map show [1..n]
      ns2 = map (\z -> read z :: Integer) ns
  in filter isPrime ns2



main = do
  let panPrimes = concat $ map nDigitPanPrimes [1..9]
      maxPPrimes = maximum $ panPrimes
  putStrLn $ "The maximum n-digit pandigital prime is " ++ show maxPPrimes ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem41.lhs
The maximum n-digit pandigital prime is 7652413.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
