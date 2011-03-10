%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem57
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 23:29:34 CDT 2010
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

It is possible to show that the square root of two can be expressed as an infinite continued fraction.

\begin{equation}
\sqrt 2 = 1 + \frac{1}{2 + \frac{1}{2 + \frac{1}{2 + \ldots }}} = 1.414213\ldots
\end{equation}

By expanding this for the first four iterations, we get:

\begin{eqnarray*}
1 + 1/2 = 3/2 = 1.5 \\
1 + 1/(2 + 1/2) = 7/5 = 1.4 \\
1 + 1/(2 + 1/(2 + 1/2)) = 17/12 = 1.41666\ldots \\
1 + 1/(2 + 1/(2 + 1/(2 + 1/2))) = 41/29 = 1.41379\ldots \\
\end{eqnarray*}

The next three expansions are $\frac{99}{70}$, $\frac{239}{169}$, and
$\frac{577}{408}$, but the eighth expansion, $\frac{1393}{985}$, is the 
first example where the number of digits in the numerator exceeds the 
number of digits in the denominator.  

In the first one-thousand expansions, how many fractions contain a 
numerator with more digits than denominator?

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Ratio

odds = filter odd [1..]

impFraction 1 = 3 / 2 :: Rational
impFraction n = 1 + 1 / (1 + impFraction (n-1))

improperFraction 1 = (3, 2) 
improperFraction n =  
  let (a, b) = improperFraction (n-1)
      b'     = a + b 
      a'     = 2 * a + (odds !! (n-2))
  in (a', b')



main = do
  let imps = take 998 $ iterate (\z -> 1 + 1 / (1 + z) :: Rational) (3/2 :: Rational)
      soln = length $ filter (\z -> length (show $ numerator z) > length (show $ denominator z)) imps
  putStrLn $ show soln


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem57.lhs
153

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
