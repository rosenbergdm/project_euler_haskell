%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem48
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Apr  2 12:36:38 CDT 2010
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

The series, $1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317$.

Find the last ten digits of the series, $1^1 + 2^2 + 3^3 + ... + 1000^{1000}$.

\section{Solution}

\colorhs
\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment


powMod b e m = b ^ e `mod` m


main = do
  let limit = 10^10
      soln = (`mod` limit) $ sum [n^n | n <- [1..1000]]
  putStrLn $ "The last ten digits of the series are " ++
             show soln ++ "."


\end{code}

\section{Result}

\begin{verbatim}

runhaskell problem48.lhs
The last ten digits of the series are 9110846700.


\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
