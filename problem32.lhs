%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem32
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sat Mar 27 14:25:13 CDT 2010
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


We shall say that an n-digit number is pandigital if it makes use of all the digits 1 to n exactly once; for example, the 5-digit number, 15234, is 1 through 5 pandigital.

The product 7254 is unusual, as the identity, $39 \times 186 = 7254$, containing multiplicand, multiplier, and product is 1 through 9 pandigital.

Find the sum of all products whose multiplicand/multiplier/product identity can be written as a 1 through 9 pandigital.
HINT: Some products can be obtained in more than one way so be sure to only include it once in your sum.


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import qualified Data.Char as C
import qualified Data.Set as Set

isPandigitalPair :: Int -> Int -> Bool
isPandigitalPair a b = (sort allchars) == "123456789" 
  where allchars = concat [show a, show b, show $ a * b]

numListToInt :: String -> Int
numListToInt ""     = 0
numListToInt (x:xs) = 10^(length xs) * x' + numListToInt xs 
  where x' = (read $ concat ["", [x]]) :: Int


intListToInt :: [Int] -> Int
intListToInt []     = 0
intListToInt (x:xs) = 10^(length xs) * x + intListToInt xs


intToIntList :: Int -> [Int]
intToIntList x
  | x < 10          = [x]
  | otherwise       = concat [(intToIntList $ x - 10 * x `div` 10), [x]]



aRange = (11, 89)

findPandigital :: Int -> [Int]
findPandigital a  = 
  let a1 = intToIntList a
      b1 = Set.toList $ Set.difference (Set.fromList [1..9]) (Set.fromList a1)
      b2 = Set.fromList $ map (intListToInt . (take (5 - length a1))) $ permutations b1
      c1 = Set.filter (\z -> (a * z > 1000) && (a * z < 10000)) b2
  in Set.toList $ Set.filter (isPandigitalPair a) c1



main = do
  let mypairs  = filter (\zz -> snd zz /= []) (map (\z -> (z, findPandigital z)) [1..89])
      triplets = concat $ map (\(a, b) -> [ (a, bb, a * bb) | bb <- b] ) $ mypairs
      uniques  = Set.fromList $ map (\(a, b, c) -> c) triplets
      unsum    = sum $ Set.toList $ uniques
  putStrLn $ "The sum of all products ab such tha the expression a * b = ab" ++
      " pandigitial is " ++ show unsum ++ "."


\end{code}

\section{Result}

\begin{verbatim}

ghc --make -O2 problem32.lhs
./problem32


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
