%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem27
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 11:26:18 CDT 2010
%% 
%% Description :
%%    Project euler problem solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\documentclass{article}
%include rosenberg.fmt
\usepackage{graphicx}
\usepackage{color}
\usepackage{pgf}

\begin{document}

\section{Problem}

Euler published the remarkable quadratic formula:

\begin{displaymath}
n^2 + n + 41
\end{displaymath}

It turns out that the formula will produce 40 primes for the consecutive 
values $n = 0$ to $39$. However, when $n = 40$, $40^2 + 40 + 41 = 40(40 + 1) + 41$
is divisible by 41, and certainly when $n = 41$, $41^2 + 41 + 41$
is clearly divisible by $41$.

Using computers, the incredible formula  $n^2 - 79n + 1601$ was discovered,
which produces 80 primes for the consecutive values $n = 0 to 79$. 
The product of the coefficients, $-79$ and $1601$, is $-126479$.

Considering quadratics of the form:

\begin{displaymath}
    n^2 + an + b, \quad |a| < 1000  \text{ and } |b| < 1000
\end{displaymath}


\begin{center}
    \footnotesize{where $|n|$ is the modulus/absolute value of $n$
    e.g. $|11| = 11$ and $|-4| = 4$}
\end{center}

Find the product of the coefficients, $a$ and $b$, for the quadratic expression 
that produces the maximum number of primes for consecutive values of n, 
starting with n = 0.


\section{Solution}


\rosenberghs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes


smallPrimes = filter (<1000) $ take 250 primes :: [Integer]

numConsecPrimes :: Integer -> Integer -> Integer
numConsecPrimes b c = fromIntegral $ length $
  takeWhile (\z -> isPrime (polyExp b c z) ) [0..]

polyExp :: Integer -> Integer -> Integer -> Integer
polyExp b c n = n^2 + b * n + c

wond2 :: Integer -> Integer
wond2 n = n^2 - 79 * n + 1601

wond1 :: Integer -> Integer
wond1 n = n^2 + n + 41

filterPass bs c zmax = bs'
  where bs' = filter (\b' -> all isPrime $ map (\z -> z^2 + b' * z + c) [0..zmax]) bs


{-
validValues c n []     = []
validValues c n (p:ps) = x : (validValues c n ps)
  where x = if isPrime (polyExp p c 
-}

problem_27 = -(2*a-1)*(a^2-a+41)
  where n = 1000
        m = head $ filter (\x->x^2-x+41>n) [1..]
        a = m-1

main = do
  args <- getArgs
  putStrLn $ "INCOMPLETE"


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem27.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
