%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem21
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 05:45:23 CDT 2010
%% 
%% Description :
%%    Project euler problem solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\documentclass{article}
%include polycode.fmt
\usepackage{graphicx}
\usepackage{color}
\usepackage{pgf}

\begin{document}

\section{Problem}


Let $d(n)$ be defined as the sum of proper divisors of $n$ (numbers less than $n$
which divide evenly into $n$).
If $d(a) = b$ and $d(b) = a$, where $a \neq b$, then $a$ and $b$ are an amicable pair and
each of $a$ and $b$ are called amicable numbers.

For example, the proper divisors of $220$ are $1, 2, 4, 5, 10, 11, 20, 22, 44, 55$
and $110$; therefore $d(220) = 284$. The proper divisors of $284$ are $1, 2, 4, 71$ and
$142$; so $d(284) = 220$.

Evaluate the sum of all the amicable numbers under 10000.


\section{Solution}

\begin{code}

import Data.Numbers
import Data.List

sumPropDivs :: Integer -> Integer
sumPropDivs x = sum $ filter (< x) (factors x)

pdList = map (sumPropDivs) [0..10000]

allPairs = filter (\(x, y) -> x /= y) $ intersect list1 list2
  where list1 = zip pdList [0..10000]
        list2 = zip [0..10000] pdList

sumAmicable = (sum . fst . unzip) allPairs

main = do
  putStrLn $ "The sum of all amicable numbers under 10000 is " ++ show sumAmicable ++ "."



\end{code}

\section{Result}

\begin{verbatim}

*Main GOA Data.List> :main 
The sum of all amicable numbers under 10000 is 31626.
it :: ()

\end{verbatim}


\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
