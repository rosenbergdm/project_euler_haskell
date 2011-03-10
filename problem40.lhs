%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem40
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Mon Mar 29 11:47:21 CDT 2010
%% 
%% Description :
%%    Project euler problem solution.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\documentclass{article}
%include colorcode.fmt
\usepackage{graphicx}
\usepackage{color}
\usepackage{pgf}
\usepackage{relsize}

\begin{document}

\section{Problem}


An irrational decimal fraction is created by concatenating the positive
integers:

\begin{center}
$0.12345678910${\Large\color{red}1}$12131415161718192021\ldots$
\end{center}

It can be seen that the 12$^{\text{th}}$ digit of the fractional part is 1.

If d represents the $n^{\text{th}}$ digit of the fractional part, find the value
of the following expression.

$$
d_1 \times d_{10} \times d_100 \times d_{1000} \times d_{10000} \times d_{100000} \times d_{1000000}
$$


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment

concatProduct :: Integral a => a -> [a] -> a
concatProduct x ys = fromIntegral (read pr :: Integer)
  where pr = (concat $ map (\z -> show (x * z) ) ys)

class Digital a where
  isPandigital :: a -> Bool

instance Digital Integer where
  isPandigital a = (sort . show) a == (take (length $ show a) "123456789")

getDigit :: Integer -> Char
getDigit x =
  let ordr  = length (takeWhile (<x) orderDigits)
      dff   = x - (orderDigits !! (fromIntegral ordr - 1))
      ndigs = length $ show $ 10 ^ ordr
      pos   = dff `mod` (fromIntegral $ ndigs)
      ns    = show (10^ordr + (dff `div` fromIntegral ndigs ))
  in (ns !! (fromIntegral pos))


orderDigits :: [Integer]
orderDigits = map (\z ->fromIntegral $ (+) 1 $ length $ concatMap show [1..(z-1)] ) $ map (10^) [1..6]


main = do
  let powersTen = map (\z -> 10^z) [2..6]
      digits    = map (\z -> read ((getDigit z):"") :: Int) powersTen 
      dProduct  = product $ concat [[1, 1], digits]
  putStrLn $ "The product of the specified digits is " ++ show dProduct ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem40.lhs
The product of the specified digits is 210.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
