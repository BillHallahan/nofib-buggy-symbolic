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

-- Mark I lazy wheel-sieve.
-- Colin Runciman (colin@cs.york.ac.uk); March 1996.
-- See article "Lazy wheel sieves and spirals of primes" (to appear, JFP).
module MainReal where

import System.IO


data Wheel = Wheel Int [Int]


primes :: [Int]
primes = sieve wheels primes squares

sieve (Wheel s ns:ws) ps qs =
-- BUG: The following line contains a bug:
  [n' | o <- s:[s*2,s*3..(head ps-1)*s],
        n <- ns,
        n'<- [n+o], noFactor n'] 
  ++
  sieve ws (tail ps) (tail qs)
  where
  noFactor = if s<=2 then const True else notDivBy ps qs

notDivBy (p:ps) (q:qs) n =
  q > n || n `mod` p > 0 && notDivBy ps qs n

squares :: [Int]
squares = [p*p | p<-primes]

wheels :: [Wheel]
wheels = Wheel 1 [1] : zipWith nextSize wheels primes 

nextSize (Wheel s ns) p =
  Wheel (s*p) ns'
  where
  ns' = [n' | o <- [0,s..(p-1)*s],
              n <- ns,
              n' <- [n+o], n'`mod`p > 0]

-- main = do
-- 	[arg] <- getArgs
-- 	print (primes!!((read arg) :: Int))
