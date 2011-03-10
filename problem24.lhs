%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem24
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 08:48:51 CDT 2010
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


A permutation is an ordered arrangement of objects. For example, 3124 is one
possible permutation of the digits 1, 2, 3 and 4. If all of the permutations
are listed numerically or alphabetically, we call it lexicographic order. The
lexicographic permutations of 0, 1 and 2 are:

\begin{center}
$012 \quad  021 \quad  102 \quad  120 \quad  201 \quad   210$
\end{center}

What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5,
6, 7, 8 and 9?

\section{Solution}

\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Debug.Trace

digitList = [0 .. 9]

\end{code}

The \emph{permutation counts} indicate the cycling period for each digit.  For example the leading digit changes every 362880 permutations.

\begin{code}

permutationCounts = 
  [ 362880, 40320, 5040, 720, 120, 24, 6, 2, 1, 1 ]


getPermutationNumber n = getDigit [] [0..9] (n-1)

getDigit digs []      n = digs
getDigit digs choices n = 
  let ind      = length digs
      thisDig  = choices !! ((fromIntegral n) `div` (permutationCounts !! ind))
      digs'    = concat [digs, [thisDig]]
      choices' = filter (/= thisDig) choices
  in getDigit digs' choices' (n - (permutationCounts !! ind) *((fromIntegral n) `div` (permutationCounts !! ind)))


main = do
  args <- getArgs
  let cmdLineArg = read (args !! 0) :: Int 
  let permString = filter (/=',') $ (reverse . tail . reverse . tail . show) $ getPermutationNumber cmdLineArg
  putStrLn $ "The " ++ show cmdLineArg ++ "th lexicographic permutation of the digits 0-9 is " ++ permString ++  "."


\end{code}


This naive method was not efficient enough.

\begin{code}

-- lexSortedPerms = sort \$ permutations digitList
-- 
-- badMain = do
--   args <- getArgs
--   let chosenIndex = read (args !! 0) :: Int
--       chosenItem = lexSortedPerms !! chosenIndex


\end{code}






\section{Result}

\begin{verbatim}

runhaskell problem24.lhs
The 1000000th lexicographic permutation of the digits 0-9 is 2783915460.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
