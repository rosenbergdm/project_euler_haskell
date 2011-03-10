%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem45
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 10:26:24 CDT 2010
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

Triangle, pentagonal, and hexagonal numbers are generated by the following formulae:

\begin{tabular}{lll}
Triangle & $T_n=n(n+1)/2$ & $1, 3, 6, 10, 15, \ldots$ \\
Pentagonal & $P_n=n(3n-1)/2$ & $1, 5, 12, 22, 35, \ldots $\\
Hexagonal & $H_n=n(2n-1) $ & $1, 6, 15, 28, 45, \ldots$
\end{tabular}

It can be verified that $T_285 = P_165 = H_143 = 40755$.

Find the next triangle number that is also pentagonal and hexagonal.



\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import qualified Data.Set as Set

tris  = Set.fromList [ (n * (n + 1)) `div` 2 | n <- [1..100000] ]
pents = Set.fromList [ (n * (3 * n - 1)) `div` 2 | n <- [1..100000] ]
hexs  = Set.fromList [ n * (2 * n - 1) | n <- [1..100000] ]


main = do
  let intsctn = Set.intersection tris $ Set.intersection pents hexs
      soln    = minimum $ Set.toList $ Set.filter (>40755) intsctn
  putStrLn $ "The next smallest triangle number is " ++ show soln ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem45.lhs
The next smallest triangle number is 1533776805.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2