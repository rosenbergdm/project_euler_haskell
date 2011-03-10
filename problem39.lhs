%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem39
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Mon Mar 29 11:47:15 CDT 2010
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


If p is the perimeter of a right angle triangle with integral length sides,
$\left(a,b,c\right)$, there are exactly three solutions for $p = 120$.

$$
  \left\{20,48,52\right\}, \{24,45,51\}, \{30,40,50\}
$$

For which value of $p < 1000$, is the number of solutions maximised?


\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import qualified Data.Set as Set

isRightTriangle :: (Integral a) => a -> a -> a -> Bool
isRightTriangle a b c = c'^2 == a'^2 + b'^2
  where [a', b', c'] = sort [a, b, c] 

isRight :: (Integral a) => Triangle a -> Bool
isRight t = c^2 == a^2 + b^2
  where [a, b, c] = sort [aa t, bb t, cc t]

data (Integral a) => Triangle a = Triangle
  { aa :: a
  , bb :: a
  , cc :: a
  } deriving (Show, Read, Ord, Eq)


triWithPerim :: (Integral a) => a -> [Triangle a]
triWithPerim p = 
  let cs = [(p `div` 3).. (p `div` 2)]
      ts = concat $ map (\c -> map (\b -> Triangle (minimum $ sort [b, c, p-b-c]) ((sort [b,c,p-b-c]) !! 1) (maximum $ sort [b, c, p-b-c])  ) [1..((p-c) `div` 2)] ) cs
  in Set.toList $ Set.fromList $ filter isRight ts

main = do
  let tris = map (\ts -> (length ts, (aa . head) ts + (bb . head) ts + (cc .  head) ts))   $ filter (\z -> z /= []) $ map triWithPerim [120..1000]
      maxVal = maximumBy (\a b -> compare (fst a) (fst b)) tris
  putStrLn $ "The largest number of solutions (" ++ (show . fst) maxVal ++ ")"
      ++ " is reached when p=" ++ (show . snd) maxVal ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem39.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
