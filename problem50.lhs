%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem50
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 14:29:21 CDT 2010
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


The prime 41, can be written as the sum of six consecutive primes:

\begin{equation}
41 = 2 + 3 + 5 + 7 + 11 + 13
\end{equation}

This is the longest sum of consecutive primes that adds to a prime below
one-hundred.

The longest sum of consecutive primes below one-thousand that adds to a prime,
contains 21 terms, and is equal to 953.

Which prime, below one-million, can be written as the sum of the most
consecutive primes?


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes

sPrimes = takeWhile (<4000) primes

bLongSeqPrimes pr = 
  let seqs = [take l (drop k pr) | k <- [0..(length pr - 1)], l <- [0..(length pr -1)]]
      lens = map (\z -> (length z, sum z)) $ filter (\z -> (sum z < 1000000) && (isPrime $ sum  z)) seqs
  in lens
  

buildr :: (a -> [b] -> [b]) -> [a] -> [b] -> [b]
buildr f [] sd     = sd
buildr f (x:xs) sd = buildr f xs (f x sd) 

main = do
  let soln = maximumBy (\x y -> compare (fst x) (fst y)) (bLongSeqPrimes sPrimes)
  putStrLn $ "The prime (< 10^6) which can be written as the sum of\n"
             ++ "the most consecutive primes is " ++ show (snd soln) ++ "."

\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem50.lhs
The prime (< 10^6) which can be written as the sum of
the most consecutive primes is 997651.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
