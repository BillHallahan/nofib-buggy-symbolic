%               Filename:  Main.lhs
%               Version :  1.4
%               Date    :  3/2/92

\section{The Main Program.}
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%M O D U L E%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{code}
module Main(main) where
\end{code}
%%%%%%%%%%%%%%%%%% I M P O R T S  /  T Y P E   D E F S %%%%%%%%%%%%%%
\begin{code}
import ChessSetList (Tile) -- partain:for hbc
import KnightHeuristic
import Queue
import System--1.3
import Char--1.3

#if __HASKELL1__ >= 5
#define fail ioError
#endif

\end{code}

%%%%%%%%%%%%%%%%%%%%% B O D Y  O F  M O D U L E %%%%%%%%%%%%%%%%%%%%%
The value of a Haskell program is the value of the identifier @main@ in the
module @Main@, and @main@ must have type @[Response] -> [Request]@. Any 
function having this type is an I/O request that 
communicates with the outside world via {\em streams} of messages. Since
Haskell is lazy language, we have the strange anomaly that the result of a 
I/O operation is a list of @Requests@'s that is generated by a list of 
@Response@'s. This characteristic is due to laziness because forcing 
evaluation on the $n^{th}$ @Response@, will in turn force evaluation on the 
$n^{th}$ request - enabling I/O to occur.

The @main@ function below uses the continuation style of I/O. Its purpose is
to read two numbers off the command line, and print out $x$ solutions to
the knights tour with a board of size $y$; where $x$ and $y$ represent
the first and second command line option respectively.

\begin{code}
main:: IO ()
main=getArgs >>= \ss ->
     if (argsOk ss) then
        putStr (printTour ss)
     else 
        fail (userError usageString)
     where
        usageString= "\nUsage: knights <board size> <no solutions> \n"
	argsOk ss = (length ss == 2) && (foldr ((&&) . all_digits) True ss)
	all_digits s = foldr ((&&) . isDigit) True s

printTour::[[Char]] -> [Char]
printTour ss
   = pp (take number (depthSearch (root size) grow isFinished))
     where
        [size,number]     = map (strToInt 0) ss
	strToInt y []     = y
	strToInt y (x:xs) = strToInt (10*y+(fromEnum x - fromEnum '0')) xs
	pp []		  = []
	pp ((x,y):xs)     = "\nKnights tour with " ++ (show x)  ++ 
	   	            " backtracking moves\n" ++ (show y) ++
			    (pp xs)

grow::(Int,ChessSet) -> [(Int,ChessSet)]
grow (x,y) = zip [(x+1),(x+1)..] (descendents y)

isFinished::(Int,ChessSet) -> Bool
isFinished (x,y) = tourFinished y

root::Int -> Queue (Int,ChessSet)
root sze= addAllFront 
             (zip [-(sze*sze)+1,-(sze*sze)+1..] 
	          (zipWith
		     startTour
		      [(x,y) | x<-[1..sze], y<-[1..sze]] 
		     (take (sze*sze) [sze,sze..])))
             createQueue
\end{code}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
The knights tour proceeds by applying a depth first search on the combinatorial
search space. The higher order function @depthSearch@ applies this search 
strategy, and returns a stream of valid search nodes. The arguments
to this function are :
\begin{itemize}
\item   A {\tt Queue} of all the possible starting positions of the search.
\item   A function that given a node in the search returns a list of valid
        nodes that are reachable from the parent node (the descendents).
\item	A function that determines if a given node in the search space is
        a valid solution to the problem (i.e is it a knights tour).
\end{itemize}

\begin{code}
depthSearch :: (Eq a) => Queue a -> (a -> [a]) -> (a -> Bool) -> Queue a
depthSearch q growFn finFn
   | emptyQueue q           = []
   | finFn (inquireFront q) = (inquireFront q):
			      (depthSearch (removeFront q) growFn finFn)
   | otherwise    	    = (depthSearch
	                         (addAllFront (growFn (inquireFront q)) 
					      (removeFront q))
	    	                 growFn
	                         finFn)
\end{code}
{\bf Note :} the above function should be abstracted out into a 
seperate search module, but as depth first search is the only 
realistic search strategy for the knights tour....

