%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem30
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sat Mar 27 10:02:55 CDT 2010
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


Surprisingly there are only three numbers that can be written as the sum of
fourth powers of their digits:

\begin{eqnarray*}
    1634 = 1^4 + 6^4 + 3^4 + 4^4
    8208 = 8^4 + 2^4 + 0^4 + 8^4
    9474 = 9^4 + 4^4 + 7^4 + 4^4
\end{eqnarray*}

As $1 = 1^4$ is not a sum it is not included.

The sum of these numbers is $1634 + 8208 + 9474 = 19316$.

Find the sum of all the numbers that can be written as the sum of fifth powers
of their digits.


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment



sumPowerDigits :: Integer -> Integer -> Integer
sumPowerDigits x p = sum $ map (^p) dgs
  where dgs = foldl (\acc z -> (read (z:"") :: Integer):acc) [] (show x)

stringToDigits :: String -> [Integer]
stringToDigits "" = []
stringToDigits (x:xs) = (read (x:"") :: Integer) : (stringToDigits xs)



main = do
  let maxPossible = 5 * 9^5
      fiveDigitMagics = filter (\z -> sumPowerDigits z 5 == z) [2..maxPossible]
  putStrLn $ "There are a total of " ++ show ( length fiveDigitMagics ) ++
    " that can be written as the sum of the fifth\npowers of their digits.  " ++
    "The sum of these numbers is " ++ show (sum fiveDigitMagics) ++ "."



\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem30.lhs
There are a total of 6 that can be written as the sum of the fifth
powers of their digits.  The sum of these numbers is 443839.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
