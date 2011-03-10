%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem52
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 17:56:39 CDT 2010
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


It can be seen that the number, 125874, and its double, 251748, contain exactly
the same digits, but in a different order.

Find the smallest positive integer, $x$, such that $2x$, $3x$, $4x$, $5x$, and
$6x$, contain the same digits.


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment


checkMultiples :: (Integral a) => a -> Bool
checkMultiples x = and $ map (\z -> x' == z) zs
  where x'  = sort $ show x
        zs' = map (x * ) [2..6]
        zs  = map (sort . show) zs'

main = do
  let soln = head $ filter checkMultiples [1..]
  putStrLn $ "The smallest integer is " ++ show soln ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem52.lhs
The smallest integer is 142857.


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
