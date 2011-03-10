%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem38
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Mon Mar 29 11:47:12 CDT 2010
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


Take the number 192 and multiply it by each of 1, 2, and 3:

\begin{eqnarray*}
    192 \times 1 = 192
    192 \times 2 &= 384
    192 \times 3 &= 576
\end{eqnarray*}

By concatenating each product we get the 1 to 9 pandigital, 192384576. We will
call 192384576 the concatenated product of 192 and (1,2,3)

The same can be achieved by starting with 9 and multiplying by 1, 2, 3, 4, and
5, giving the pandigital, 918273645, which is the concatenated product of 9 and
(1,2,3,4,5).

What is the largest 1 to 9 pandigital 9-digit number that can be formed as the
concatenated product of an integer with $(1,2, \ldots , n)$ where $n > 1$?

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment


-- Generates the 'String' concatenated products of an 'Integer' and a list
concatProduct :: Integral a => a -> [a] -> a
concatProduct x ys = fromIntegral (read pr :: Integer)
  where pr = (concat $ map (\z -> show (x * z) ) ys)

class Digital a where
  isPandigital :: a -> Bool

instance Digital Integer where
  isPandigital a = (sort . show) a == "123456789"

instance Digital Pair where
  isPandigital pr = digs == "123456789"
    where (x, y) = (xx pr, yy pr)
          digs   = sort $ (sort . concat) $ map (show . (x*)) [1..y]



-- As per the question's criteria
data Pair = Pair
  { xx :: Integer
  , yy :: Integer
  } deriving (Show, Eq, Ord, Read)

main = do
  let cps = (maximum $ concat $ map (\x -> (filter (isPandigital) $ map (\z -> (concatProduct x [1..z])) [1..9])) [1..10000]) :: Integer
  putStrLn $ "The maximum nine-digit concatenated product is " ++ (show cps) ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem38.lhs
The maximum nine-digit concatenated product is 932718654.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
