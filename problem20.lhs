%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem20
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 01:27:11 CDT 2010
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


$n!$ means $n \times (n-1) \times  \ldots  \times  3 \times 2 \times  1$

Find the sum of the digits in the number $100!$


\section{Solution}

\begin{code}

sumDigits :: [Char] -> Int
sumDigits "" = 0
sumDigits (x:xs) = (read (x:"") :: Int) + sumDigits xs

main = do
  let sumdigs = (sumDigits . show) $ foldl (\acc z -> acc * z) 1 [1..100]
  putStrLn $ "The sum of the digits in the expansion of 100! is " ++ show sumdigs ++ "."

\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem20.lhas
The sum of the digits in the expansion of 100! is 648.

\end{verbatim}


\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
