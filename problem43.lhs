%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem43
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Thu Apr  1 19:34:03 CDT 2010
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


The number, 1406357289, is a 0 to 9 pandigital number because it is made up of
each of the digits 0 to 9 in some order, but it also has a rather interesting
sub-string divisibility property.

Let $d_1$ be the 1$^{\text{st}}$ digit, $d_2$ be the 2$^{\text{nd}}$ digit, and so on. In
this way, we note the following:

\begin{itemize}
  \item $d_2d_3d_4=406$ is divisible by 2
  \item $d_3d_4d_5=063$ is divisible by 3
  \item $d_4d_5d_6=635$ is divisible by 5
  \item $d_5d_6d_7=357$ is divisible by 7
  \item $d_6d_7d_8=572$ is divisible by 11
  \item $d_7d_8d_9=728$ is divisible by 13
  \item $d_8d_9d_{10}=289$ is divisible by 17
\end{itemize}

Find the sum of all 0 to 9 pandigital numbers with this property.


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes

checkDivisibility :: Integer -> Integer -> Bool
checkDivisibility n d = n' `mod` d' == 0
  where n' = read (take 3 $ drop (fromIntegral (d-1)) $ show n) :: Int
        d' = fromIntegral (primes !! (fromIntegral (d-2))) :: Int


nDigitPans :: Int -> [Integer]
nDigitPans n = map (\z -> read z :: Integer) (permutations ( take n "0123456789"))



main = do
  let ns     = nDigitPans 10
      magics = foldl (\acc z -> filter (\k -> checkDivisibility k z) acc) ns [2..8]
      msum   = sum magics
  putStrLn $ "The sum of all such numbers is "  ++ show msum ++ "." 


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem43.lhs
The sum of all such numbers is 16695334890.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
