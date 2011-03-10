%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem17
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Thu Mar 25 20:22:48 CDT 2010
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


If the numbers 1 to 5 are written out in words: one, two, three, four, five,
then there are $3 + 3 + 5 + 4 + 4 = 19$ letters used in total.

If all the numbers from 1 to 1000 (one thousand) inclusive were written out in
words, how many letters would be used?


NOTE: Do not count spaces or hyphens. For example, 342 (three hundred and
forty-two) contains 23 letters and 115 (one hundred and fifteen) contains 20
letters. The use of "and" when writing out numbers is in compliance with
British usage.


\section{Solution}

\begin{code}

import qualified Data.Map as Map
import Data.Maybe

digitMap = Map.fromList
  [ (1, "one")
  , (2, "two")
  , (3, "three")
  , (4, "four")
  , (5, "five")
  , (6, "six")
  , (7, "seven")
  , (8, "eight")
  , (9, "nine")
  , (10, "ten")
  , (11, "eleven")
  , (12, "twelve")
  , (13, "thirteen")
  , (14, "fourteen")
  , (15, "fifteen")
  , (16, "sixteen")
  , (17, "seventeen")
  , (18, "eighteen")
  , (19, "nineteen")
  ]

tensMap = Map.fromList
  [ (20, "twenty")
  , (30, "thirty")
  , (40, "forty")
  , (50, "fifty")
  , (60, "sixty")
  , (70, "seventy")
  , (80, "eighty")
  , (90, "ninety")
  ]

numberToString :: Integer -> String
numberToString x
  | x == 1000   = "one thousand "
  | x == 0      = ""
  | x >= 100    = (fromJust $ Map.lookup (x `div` 100) digitMap) ++ " hundred" ++ (addAnd x) ++ (numberToString (x - 100 * (x `div` 100) ))
  | x >= 20     = (fromJust $ Map.lookup ((x `div` 10) * 10) tensMap) ++ " " ++ (numberToString (x - 10 * (x `div` 10)))
  | otherwise   = fromJust $ Map.lookup x digitMap

addAnd :: Integer -> String
addAnd x
  | x - 100 * (x `div` 100) == 0    = ""
  | otherwise                       = " and "

main = do
    let numSum = sum $ map (length . ( filter (\z -> z /= ' ') ) . numberToString ) [1..1000]
    putStrLn  $ "The total number of non-space, non-hyphen characters in the list 'one', 'two', ..., 'one-thousand' is " ++ (show numSum) ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem17.lhs
The total number of non-space, non-hyphen characters in the list 'one', 'two', ..., 'one-thousand' is 21124.

\end{verbatim}

The total number of non-space, non-hyphen characters in the list \emph{one, two, $\ldots$, one-thousand} is 21124.

\end{document}
