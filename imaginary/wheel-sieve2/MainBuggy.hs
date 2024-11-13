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

-- Mark II lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).
module MainBuggy where

import System.IO

primes :: [Int]
primes = spiral wheels primes squares

spiral (Wheel s ms ns:ws) ps qs =
  foldr turn0 (roll s) ns
  where
-- BUG: The following line contains a bug: 
  roll o = foldr (turn o) (foldr (turn o) (roll s) ns) ms
-- CORRECT --    roll o = foldr (turn o) (foldr (turn o) (roll (o+s)) ns) ms
  turn0  n rs =
    if n<q then n:rs else sp
  turn o n rs =
    let n' = o+n in
    if n'==2 || n'<q then n':rs else dropWhile (<n') sp
  sp = spiral ws (tail ps) (tail qs)
  q = head qs

squares :: [Int]
squares = [p*p | p <- primes]

data Wheel = Wheel Int [Int] [Int]

wheels :: [Wheel]
wheels = Wheel 1 [1] [] :
         zipWith3 nextSize wheels primes squares 

nextSize (Wheel s ms ns) p q =
  Wheel (s*p) ms' ns'
  where
  (xs, ns') = span (<=q) (foldr turn0 (roll (p-1) s) ns)
  ms' = foldr turn0 xs ms
  roll 0 _ = []
  roll t o = foldr (turn o) (foldr (turn o) (roll (t-1) (o+s)) ns) ms
  turn0  n rs =
    if n`mod`p>0 then n:rs else rs
  turn o n rs =
    let n' = o+n in
    if n'`mod`p>0 then n':rs else rs

-- main = do
-- 	[arg] <- getArgs
-- 	print (primes!!((read arg) :: Int))

