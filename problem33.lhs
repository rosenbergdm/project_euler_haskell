%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem33
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sun Mar 28 10:49:01 CDT 2010
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

The fraction $\frac{49}{98}$ is a curious fraction, as an inexperienced
mathematician in attempting to simplify it may incorrectly believe that
$\frac{49}{98} = \frac{4}{8}$, which is correct, is obtained by cancelling the 9s.

We shall consider fractions like, $\frac{30}{50} = \frac{3}{5}$, to be trivial examples.

There are exactly four non-trivial examples of this type of fraction, less than one in value, and containing two digits in the numerator and denominator.

If the product of these four fractions is given in its lowest common terms, find the value of the denominator.


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment


justDigit :: Int -> Int -> Int
justDigit i x = read (x':"") :: Int
  where x' = (show x) !! (i - 1) 


isCuriousFrac :: Int -> Int -> Bool
isCuriousFrac a b =
  let fx1         = if justDigit 2 a == justDigit 1 b
                      then (justDigit 1 a, justDigit 2 b)
                      else (1, 1)
      fx2         = if justDigit 1 a == justDigit 2 b
                      then (justDigit 2 a, justDigit 1 b)
                      else (1, 1)
      equivFracs  = filter (\(x, y) -> (fromIntegral x / fromIntegral y) ==
                        (fromIntegral a / fromIntegral b)) [fx1, fx2]
  in ((length equivFracs) > 0)


main = do
  let curFrac = filter (\(x, y) -> (isCuriousFrac x y) && (x < y))
                        [ (x, y) | x <- [10..99], y <- [10..99] ]
      curFractions = map (\(x, y) -> (fromIntegral x / fromIntegral y) ::
                        Rational ) curFrac
  putStrLn $ "The product of the these four `curious' fractions ( " ++ 
    show curFractions ++ " is " ++ (show . product) curFractions ++ "."



\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem33.lhs
The product of the these four `curious' fractions ( [1 % 4,1 % 5,2 % 5,1 % 2] is 1 % 100.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
