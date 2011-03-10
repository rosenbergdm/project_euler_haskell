%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem56
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 23:23:47 CDT 2010
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


A googol $10^{100}$ is a massive number: one followed by one-hundred zeros;
$100^{100}$ is almost unimaginably large: one followed by two-hundred zeros.
Despite their size, the sum of the digits in each number is only 1.

Considering natural numbers of the form, $a^b$, where $a, b < 100$, what is the
maximum digital sum?

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

sumDigits x = foldl (\acc z -> acc + (read (z:"") :: Integer)) 0 (show x) 



main = do
  let soln = maximum [ sumDigits $ a^b | a <- [1..100], b <- [1..100] ]
  putStrLn $ "The maximum digital sum is " ++ show soln ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem56.lhs
The maximum digital sum is 972.


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
