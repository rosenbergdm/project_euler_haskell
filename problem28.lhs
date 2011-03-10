%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Module      : problem28
%% Copyright   : (c) 2010 David M. Rosenberg
%% License     : BSD3
%% 
%% Maintainer  : David Rosenberg <rosenbergdm@uchicago.edu>
%% Stability   : experimental
%% Portability : portable
%% Created     : Fri Mar 26 23:28:02 CDT 2010
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


Starting with the number 1 and moving to the right in a clockwise direction a 5
by 5 spiral is formed as follows:\\

\begin{center}
\begin{tabular}{c c c c c}  
  {\color{red}21} & 22 & 23 & 24 & {\color{red}25} \\
  20  &{\color{red} 7}  & 8  & {\color{red} 9} & 10 \\
  19  &6  & {\color{red} 1}  & 2 & 11 \\
  18  &{\color{red} 5}  & 4  & {\color{red} 3} & 12 \\
  {\color{red} 17} & 16 & 15 & 14 & {\color{red} 13} \\
\end{tabular}
\end{center}

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?






\section{Solution}
\colorhs
% \tightcolorhs
% \barhs
% \rosenberghs


\begin{code}

import Data.List
import qualified Data.Map as Map
import Data.Maybe
import System.Environment


numsInLevel :: (Integral a) => a -> a
numsInLevel 0 = 1
numsInLevel n = (2 * n + 1)^2 - (2 * n - 1)^2

sepAtLevel :: (Integral a) => a -> a
sepAtLevel 0 = 0
sepAtLevel 1 = 1
sepAtLevel n = 2 + (sepAtLevel $ n - 1)


pickDiagVals :: (Integral a) => [a] -> Int -> [a]
pickDiagVals []   sep = []
pickDiagVals vals sep = v:(pickDiagVals vals' sep)
  where vals' = drop (sep + 1) vals
        v     = head $ drop sep vals


mkLevel stnum lv = ((lv, nums), diags)
  where
    nums  = [stnum..(stnum + (numsInLevel lv) - 1)]
    sep   = sepAtLevel lv
    diags = pickDiagVals nums sep

mkSpiral nmax sp 
  | length sp == nmax       = sp
  | otherwise               = mkSpiral nmax (concat [sp, [thisSpiral]])
    where startAt    = if sp == []
                         then 1
                         else 1 + ((last . snd . fst . last) sp)
          lv         = length sp
          thisSpiral = mkLevel startAt lv

sumSpiral sp = (sum . concat) $ map snd sp


main = do
  let mySum = sumSpiral $ mkSpiral 501 []
  putStrLn $ "The sum of the diagonals on the 1001 x 1001 spiral grid is " ++ show mySum ++ "."


\end{code}

\section{Result}

\begin{verbatim}

*Main GOA> :main 
The sum of the diagonals on the 1001 x 1001 spiral grid is 669171001.

\end{verbatim}

\end{document}

% vim: ft=lhaskell softtabstop=2 shiftwidth=2
