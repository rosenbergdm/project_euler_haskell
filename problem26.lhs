%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem26
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 10:47:38 CDT 2010
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

A unit fraction contains a $1$ in the numerator. The decimal representation 
of the unit fractions with denominators 2 to 10 are given:

\begin{eqnarray*}
    \frac{1}{2}	&= 	0.5 \\
    \frac{1}{3}	&= 	0.\overline{3} \\
    \frac{1}{4}	&= 	0.25 \\
    \frac{1}{5}	&= 	0.2 \\
    \frac{1}{6}	&= 	0.1 \overline{6} \\
    \frac{1}{7}	&= 	0. \overline{142857} \\
    \frac{1}{8}	&= 	0.125 \\
    \frac{1}{9}	&= 	0. \overline {1} \\
    \frac{1}{10}	&= 	0.1 
\end{eqnarray*}

Where $0.1\bar{6}$ means $0.166666\ldots$ and has a 1-digit recurring 
cycle. It can be seen that $\frac{1}{7}$ has a 6-digit recurring cycle.

Find the value of $d < 1000$ for which $\frac{1}{d}$ contains the 
longest recurring cycle in its decimal fraction part.


\section{Solution}

\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Ord


remainders d 0 rs = 0
remainders d r rs = 
  let r' = r `mod` d
  in case elemIndex r' rs of
              Just i  -> i + 1
              Nothing -> remainders d (10*r') (r':rs)

recurringCycle d = remainders d 10 []


ordInt :: (Integral a) => a -> a -> a
ordInt r n = (snd . head) $ 
  filter (\(x, y) -> x == 0) [((r^k-1) `mod` n, k) | k <- [1..n]]

repeatPeriod n b = ordInt b n'
  where n' = product $ 
          filter (\z -> z `notElem` [2, 5]) (1:(primeFactors n))

coprimeNums k = 
  map (\zz ->  product $ 
    filter (\z -> z /= 2 && z /= 5) (1:(primeFactors zz)) ) [1..k]

repeatLengths xs = map (\x -> (x, repeatPeriod x 10)) xs  
maxEntry rlpairs = fst $ 
  maximumBy (\(_, a) (_, b) -> compare a b) rlpairs


-- below is someone else's implementation I put in afterwards 
-- to compare to my own.

nums = [ n | n <- [3,5..], n `mod` 5 /= 0 ]
 
period n =
    head $ [ p | p <- [1..], (10^p - 1) `mod` n == 0 ]
 
answer =
    fst $
    maximumBy (\(_,a) (_,b) -> compare a b) $
    map (\n -> (n,period n)) $
    takeWhile (<1000) nums
  

main = do
  let cn = coprimeNums 1000
      rl = repeatLengths cn
      me = maxEntry rl
  putStrLn $ "The longest repeating decimal found in the set " ++ 
    "[1/2, 1/3, ..., 1/1000] is given by the expansion of 1/" ++ 
    show me ++ ", which has a repeating sequence of 982 digits."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem26.lhs
The longest repeating decimal found in the set [1/2, 1/3, ..., 1/1000] 
is given by the expansion of 1/983, which has a repeating sequence of 
982 digits.

\end{verbatim}

The longest repeating decimal found in the set 

$$
\{ \frac{1}{2}, \frac{1}{3}, ..., \frac{1}{1000} \} 
$$

is given by the expansion of $\frac{1}{983}$, which has a 
repeating sequence of 982 digits.



\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
