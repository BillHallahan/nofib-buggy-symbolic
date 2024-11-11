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

-- There was a lot of discussion about various ways of computing
-- Bernouilli numbers (whatever they are) on haskell-cafe in March 2003
-- Here's one of the programs.

-- It's not a very good test, I suspect, because it manipulates big integers,
-- and so probably spends most of its time in GMP.  
module MainBuggy where

import Data.Ratio
import System.Environment

-- powers = [[r^n | r<-[2..]] | n<-1..]
powers = [2..] : map (zipWith (*) (head powers)) powers

-- powers = [[(-1)^r * r^n | r<-[2..]] | n<-1..]
neg_powers = 
-- BUG: The following line contains a bug:
  map (zipWith (\n x -> if n then -x else x) (iterate not True)) powers
-- CORRECT --   map (zipWith (\n x -> if n then x else -x) (iterate not True)) powers

pascal:: [[Integer]]
pascal = [1,2,1] : map (\line -> zipWith (+) (line++[0]) (0:line)) pascal

bernoulli 0 = 1
bernoulli 1 = -(1%2)	
bernoulli n | odd n = 0
bernoulli n = 
   (-1)%2 
     + sum [ fromIntegral ((sum $ zipWith (*) powers (tail $ tail combs)) - 
                            fromIntegral k) %
             fromIntegral (k+1)
     | (k,combs)<- zip [2..n] pascal]
  where powers = (neg_powers!!(n-1))

-- main = do
--  [arg] <- getArgs
--  let n = (read arg)::Int
--  putStr $ "Bernoulli of " ++ (show n) ++ " is "
--  print (bernoulli n)
