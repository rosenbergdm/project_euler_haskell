%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem55
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 23:08:07 CDT 2010
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


If we take 47, reverse and add, $47 + 74 = 121$, which is palindromic.

Not all numbers produce palindromes so quickly. For example,

\begin{eqnarray*}
349 + 943 = 1292
1292 + 2921 = 4213
4213 + 3124 = 7337
\end{eqnarray*}

That is, 349 took three iterations to arrive at a palindrome.

Although no one has proved it yet, it is thought that some numbers, like 196,
never produce a palindrome. A number that never forms a palindrome through the
reverse and add process is called a Lychrel number. Due to the theoretical
nature of these numbers, and for the purpose of this problem, we shall assume
that a number is Lychrel until proven otherwise. In addition you are given that
for every number below ten-thousand, it will either 
\begin{enumerate}
  \item become a palindrome in less than fifty iterations, or, 
  \item no one, with all the computing power that exists, has managed so far
  to map it to a palindrome. 
\end{enumerate}
In fact, 10677 is the first number to be shown to require over fifty iterations
before producing a palindrome: 4668731596684224866951378664 
(53 iterations, 28-digits).

Surprisingly, there are palindromic numbers that are themselves Lychrel
numbers; the first example is 4994.

How many Lychrel numbers are there below ten-thousand?

\textbf{NOTE:} Wording was modified slightly on 24 April 2007 to emphasise the
theoretical nature of Lychrel numbers.

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes


isPalindrome x = (show x) == (reverse $ show x)

isLychrel x = isLychrel' 50 x

isLychrel' 0 x = True
isLychrel' n x = 
  let x' = x + (read (reverse $ show x) :: Integer)
  in if isPalindrome x'
      then False
      else isLychrel' (n-1) x'


main = do
  let soln = length $ filter isLychrel [1..9999]
  putStrLn $ "There are " ++ show soln ++ " Lychrel numbers below " ++
             "ten thousand."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem55.lhs
There are 249 Lychrel numbers below ten thousand.


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
