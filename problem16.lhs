%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem16
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Thu Mar 25 19:17:31 CDT 2010
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


$ 2^{15} = 32768 $ and the sum of its digits is $3 + 2 + 7 + 6 + 8 = 26$.

What is the sum of the digits of the number $2^{1000}$?





\section{Solution}

\begin{code}

import Data.Map

sumDigits :: [Char] -> Int
sumDigits "" = 0
sumDigits (x:xs) = (read (x:"") :: Int) + sumDigits xs

pDigitsSum :: (Integral a) => a -> IO ()
pDigitsSum 1 = do
    putStrLn $ "2^1   = 2              --> 2"
    return ()
pDigitsSum n = do
    putStrLn $ "2^" ++ show n ++ "     = " ++ show (2^n) ++ "              --> " ++ (show . sumDigits . show . (2^)) n
    pDigitsSum (n-1)
    return ()

main = do
    putStrLn $ "The sum of the digits of 2^1000 = " ++ show (sumDigits $ show $ 2^1000) ++ "."

\end{code}

\section{Result}

\begin{verbatim}
./runhaskell problem16.lhs

The sum of the digits of 2^1000 = 1366.

\end{verbatim}

The sum of the digits is $1366$.

\end{document}
