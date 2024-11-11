---------------------------------------------------------
--       WARNING: THIS PROGRAM CONTAINS A BUG!!!       --
--                                                     --
--  This program belongs to the faulty nofib library   --
--      and contains a bug to benchmark debuggers      --
--                                                     --
---------------------------------------------------------
--                                                     --
--  "The faulty nofib library" is a  collection of     --
--  Haskell programs from the 'nofib' benchmark suite  --
--                                                     --
--  Faults are always marked with a comment: "BUG"     --
--  The commented correct line appears after the       --
--  faulty line marked with "CORRECT"                  --
--                                                     --
--  We welcome any comment or improvement about        --
--  bugs. You can send them to:                        --
--        Josep Silva (jsilva@dsic.upv.es)             --
--                                                     --
---------------------------------------------------------
--                                                     --
--  There are three kinds of bugs depending on their   --
--  consequences:                                      --
--  1) Bugs that produce an incorrect result           --
--  2) Bugs that produce non-termination               --
--  3) Bugs that produce an exception (e.g. div by 0)  --
--                                                     --
--  This program contains a bug of tipe 2 or 3         --
--  "ERROR - C stack overflow"                         --
---------------------------------------------------------

-- !!! count the number of solutions to the "n queens" problem.
-- (grabbed from LML dist)

import System


main = do
	[arg] <- getArgs
	print $ nsoln $ read arg

nsoln nq = length (gen nq)
 where
    safe :: Int -> Int -> [Int] -> Bool
    safe x d []    = True
    safe x d (q:l) = x /= q && x /= q+d && x /= q-d && safe x (d+1) l

    gen :: Int -> [[Int]]
    gen 0 = [[]]
-- BUG: The following line contains a bug:
    gen n = [ (q:b) | b <- gen (n), q <- [1..nq], safe q 1 b]
-- CORRECT --  gen n = [ (q:b) | b <- gen (n-1), q <- [1..nq], safe q 1 b]
