\documentclass{article}
%include polycode.fmt



\begin{document}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem14
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : 03/21/10
%% 
%% Description :
%%    DESCRIPTION HERE.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Problem}

The following iterative sequence is defined for the set of positive      
integers:                                                               

\begin{eqnarray}
    n & \rightarrow n/2 (n \text{ is even}) \\
    n & \rightarrow 3n + 1 (n \text { is odd}) \\
\end{eqnarray}
                                                                        
Using the rule above and starting with 13, we generate the following    
sequence:                                                               

$$
13 \rightarrow 40 \rightarrow 20 \rightarrow 10 \rightarrow 5 \rightarrow 16 \rightarrow 8 \rightarrow 4 \rightarrow 2 \rightarrow 1                     
$$                                                                        

It can be seen that this sequence (starting at 13 and finishing at 1)   
contains 10 terms. Although it has not been proved yet (Collatz         
Problem), it is thought that all starting numbers finish at 1.          
                                                                        
\textbf{Which starting number, under one million, produces the longest chain?}   
                                                                        
NOTE: Once the chain starts the terms are allowed to go above one       
million.                                                                

\section{Solution}

\begin{code}
import System.Environment
import Debug.Trace 
import Data.Map as Map 
import Control.Monad.State.Lazy as State


type StateMap a b = State ( Map a b ) b
memoizeM :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM t x = evalState (f x) Map.empty where
  g x = do y <- t f x 
           m <- get
           put $ Map.insert x y m 
           newM <- get
           return $ trace ("Map now contains" ++ Map.showTree newM) y 
  f x = get >>= \m -> maybe (g x) return (Map.lookup x m)

twond m n = trace ("wondM called with " ++ show m ++ " returning " ++ show n) n

wondM :: Monad m => (Integer -> m Integer) -> Integer -> m Integer
wondM f' 1 = return $ twond 1 1
wondM f' n = do
  let n'  = if even n
                then (n `div` 2)
                else (3 * n + 1)
  n'' <- f' n'
  return $ twond n (1 + n'')

wond n = memoizeM wondM n

memoizeM' :: (Show a, Show b, Ord a) => 
            ((a -> StateMap a b) -> (a -> StateMap a b)) -> (a -> b)
memoizeM' t x = evalState (f x) Map.empty where
  g x = do y <- t f x 
           m <- get
           put $ Map.insert x y m 
           newM <- get
           return y 
  f x = get >>= \m -> maybe (g x) return (Map.lookup x m)


wondM' :: Monad m => (Integer -> m Integer) -> Integer -> m Integer
wondM' f' 1 = return 1
wondM' f' n = do
  let n'  = if even n
                then (n `div` 2)
                else (3 * n + 1)
  n'' <- f' n'
  return (1 + n'')

wond' n = memoizeM' wondM' n

main = do
  args <- getArgs
  let n = read (args !! 0) :: Integer
  let maxOver = maximum $ Prelude.map wond' [1..n]
  let maxWith = Prelude.filter (\z -> wond' z == maxOver) [1..n]
  putStrLn $ "The maximum value achieved is " ++ show maxOver ++ " which is found using n = " ++ show maxWith ++ "." 
\end{code}

\section{Results}
Using this method, I found...

\begin{verbatim}

./prob14 1000000
The maximum value achieved is 525 which is found using n = [837799].

\end{verbatim}

\end{document}


