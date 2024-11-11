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
--  This program contains a bug of tipe 1              --
---------------------------------------------------------

-- LML original: Sandra Foubister, 1990
-- Haskell translation: Colin Runciman, May 1991

module Diff(diff, bcroot, square) where

square :: Int -> Int
-- BUG: The following line contains a bug
square n = n - n
-- CORRECT -- square n = n*n

diff :: Int -> Int -> Int
diff a b = if a>b then a-b else b-a

bcroot :: Int -> Int
bcroot n = root' 0 n
	   where root' a b = if a+1>=b then b
                             else if s<n then root' m b
                             else if n<s then root' a m
                             else m
	                     where
                             m = (a+b) `div` 2
                             s = m*m



