%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem46
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 10:42:45 CDT 2010
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


It was proposed by Christian Goldbach that every odd composite number can be
written as the sum of a prime and twice a square.

\begin{eqnarray*}
9 = 7 + 2 \times 1^2 \\
15 = 7 + 2 \times 2^2 \\
21 = 3 + 2 \times 3^2 \\
25 = 7 + 2 \times 3^2 \\
27 = 19 + 2 \times 2^2 \\
33 = 31 + 2 \times 1^2
\end{eqnarray*}

It turns out that the conjecture was false.

What is the smallest odd composite that cannot be written as the sum of a prime
and twice a square?



\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes

oddComposites n = filter (\z -> (not $ isPrime z) && (odd z)) [1..n] 


compB n = filter (<n) [2 * k^2 | k <- [1..(round $ sqrt $ fromIntegral n)] ]

checkOddComposite :: Integer -> Bool
checkOddComposite x = 
  let compAs = map ((-) x) (compB x)
      solns  = filter isPrime compAs
  in length solns == 0

main = do
  let ocs = oddComposites 10000
      soln = minimum $ filter checkOddComposite ocs
  putStrLn $ "The smallest odd composite that cannot be written as the " ++
             "sum of a prime and twice a squre is " ++ show soln ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem46.lhs
The smallest odd composite that cannot be written as the sum 
of a prime and twice a squre is 5777.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
