%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem31
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Sat Mar 27 10:32:54 CDT 2010
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


In England the currency is made up of pound, (\pounds), and pence, (\emph{p}), and there are
eight coins in general circulation:

\begin{center}
    1\emph{p}, 2\emph{p}, 5\emph{p}, 10\emph{p}, 20\emph{p}, 50\emph{p},
    \pounds 1 (100\emph{p}) and \pounds 2 (200\emph{p}).
\end{center}

It is possible to make \pounds 2 in the following way:

    1 $\times$ 1 \pounds  + 1 $\times$ 50 \emph{p} + 2 $\times$ 20 \emph{p} + 
    1 $\times$ 5 \emph{p} + 1 $\times$ 2 \emph{p} + 3 $\times$ 1 \emph{p}
    

How many different ways can \pounds 2 be made using any number of coins?




\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import qualified Data.Set as Set

denoms = [ 0.01, 0.02, 0.05, 0.10, 0.20, 0.50, 1.0, 2.0 ]

data Coin = P1 | P2 | P5 | P10 | P20 | P50 | L100 | L200
  deriving (Show, Ord, Eq, Enum)

coinSet = Map.fromList
  [ (P1, 0) , (P2, 0) , (P5, 0) , (P10, 0) , (P20, 0) , (P50, 0) , (L100, 0) , (L200, 0) ] 


valueCoins :: Map.Map Coin Integer -> Double
valueCoins cs = fromIntegral csum
  where csum = 1 * (maybe 0 id (Map.lookup P1 cs)) +  
               2 * (maybe 0 id (Map.lookup P2 cs)) +
               5 * (maybe 0 id (Map.lookup P5 cs)) +
               10 * (maybe 0 id (Map.lookup P10 cs)) +
               20 * (maybe 0 id (Map.lookup P20 cs)) +
               50 * (maybe 0 id (Map.lookup P50 cs)) +
               100 * (maybe 0 id (Map.lookup L100 cs)) +
               200 * (maybe 0 id (Map.lookup L200 cs))

addCoin :: Map.Map Coin Integer -> Coin -> Map.Map Coin Integer
addCoin cs c = cs'
  where cs' = Map.adjust (+1) c cs

nPermCoins val css = css' 
  where curVal  = valueCoins $ (head $ Set.elems css)
        addP1   = Set.map (\z -> addCoin z P1) css  
        addP2   = Set.map (\z -> addCoin z P2) css
        addP5   = Set.map (\z -> addCoin z P5) css
        addP10  = Set.map (\z -> addCoin z P10) css
        addP20  = Set.map (\z -> addCoin z P20) css
        addP50  = Set.map (\z -> addCoin z P50) css
        addL100 = Set.map (\z -> addCoin z L100) css
        addL200 = Set.map (\z -> addCoin z L200) css
        css'
          | curVal  == val       = css
          | val - curVal == 0.05 = addP1
          | val - curVal == 0.10 = foldl Set.union Set.empty [addP1, addP5]
          | val - curVal == 0.20 = foldl Set.union Set.empty [addP1, addP5, addP10] 
          | val - curVal == 0.50 = foldl Set.union Set.empty [addP1, addP5, addP10, addP20]
          | val - curVal == 0.00 = foldl Set.union Set.empty [addP1, addP5, addP10, addP20, addP50]
          | val - curVal == 1.00 = foldl Set.union Set.empty [addP1, addP5, addP10, addP20, addP50, addL100]
          | otherwise            = foldl Set.union Set.empty [addP1, addP5, addP10, addP20, addP50, addL100, addL200]


problem_31_0 = ways [1,2,5,10,20,50,100,200] !!200
  where ways [] = 1 : repeat 0
        ways (coin:coins) =n 
          where n = zipWith (+) (ways coins) (replicate coin 0 ++ n)

--A beautiful solution, making usage of laziness and recursion to implement a dynamic programming scheme, blazingly fast despite actually generating the combinations and not only counting them :

coins_0 = [1,2,5,10,20,50,100,200]
 
combinations = foldl (\without p ->
                          let (poor,rich) = splitAt p without
                              with = poor ++ zipWith (++) (map (map (p:)) with)
                                                          rich
                          in with
                     ) ([[]] : repeat [])
 
problem_31_1 = length $ combinations coins_0 !! 200

-- The above may be a beautiful solution, but I couldn't understand it without major mental gymnastics. I would like to offer the following, which I hope will be easier to follow for ordinary mentats -- HenryLaxen 2008-02-22

coins = [1,2,5,10,20,50,100,200]
 
withcoins 1 x = [[x]]
withcoins n x = concatMap addCoin [0 .. x `div` coins!!(n-1)]
  where addCoin k = map (++[k]) (withcoins (n-1) (x - k*coins!!(n-1)) )
 
problem_31_2 = length $ withcoins (length coins) 200




main = do
  args <- getArgs
  putStrLn $ "INCOMPLETE" 


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem31.lhs


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
