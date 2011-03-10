%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem51
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 17:15:31 CDT 2010
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


By replacing the $1^{\text{st}}$ digit of $\cdot3$, it turns out that six of
the nine possible values: 13, 23, 43, 53, 73, and 83, are all prime.

By replacing the $3^{\text{rd}}$ and $4^{\text{th}}$ digits of $56\cdot\cdot3$
with the same digit, this 5-digit number is the first example having seven 
primes among the ten generated numbers, yielding the family: 56003, 56113, 
56333, 56443, 56663, 56773, and 56993. Consequently 56003, being the first 
member of this family, is the smallest prime with this property.

Find the smallest prime which, by replacing part of the number (not 
necessarily adjacent digits) with the same digit, is part of an eight
prime value family.  

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment
import Data.Numbers
import Data.Numbers.Primes
import qualified Data.Set as Set

primesByReplacement :: (Integral a) => [a] -> [Integer]
primesByReplacement x = filter isPrime potPrimes
  where potPrimes = map (\z -> replaceStars x z) [1..9] 

sPrimes = takeWhile (<20000) primes


buildFamilies :: (Integral a) => a -> [([Int], [Integer])]
buildFamilies x =
  let digs = map (\z -> (read (z:"") :: Int)) (Set.toList $ (Set.fromList . show) x)
      fams = map (\z -> replaceWithStar x z) digs
  in map (\z -> (z, primesByReplacement z)) fams


replaceWithStar :: (Integral a, Show a) => a -> Int -> [Int]
replaceWithStar x d = ds'
  where ds = map (\z -> read (z:"") :: Int) (show x)
        ds' = map (\k -> if k == d then -1 else k) ds


replaceStars :: (Integral a) => [a] -> a -> Integer
replaceStars ds v = read (concat $ map show (replaceStars' ds [] v)) :: Integer
  where replaceStars' [] ds' v = ds'
        replaceStars' (d:ds) ds' v = 
          let d'   = if d < 0 then v else d
              ds'' = concat [ds', [d']]
          in replaceStars' ds ds'' v


main = do
  let soln  = head $ 
          filter (\x -> length (snd x) >= 8) $ 
          concat (map buildFamilies primes)
      soln' = minimum $ snd soln 
  putStrLn $ "The smallest prime that is a member of an eight-prime \n" ++
             "family is " ++ show soln' ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem51.lhs
The smallest prime that is a member of an eight-prime 
family is 121313.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
