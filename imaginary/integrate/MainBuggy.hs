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

module MainBuggy where

import System.IO

integrate1D :: Double -> Double -> (Double->Double) -> Double
integrate1D l u f =
  let  d = (u-l)/8.0 in
     d * sum 
      [ (f l)*0.5,
        f (l+d),
        f (l+(2.0*d)),
        f (l+(3.0*d)),
        f (l+(4.0*d)),
        f (u-(3.0*d)),
        f (u-(2.0*d)),
        f (u-d),
        (f u)*0.5]

integrate2D l1 u1 l2 u2 f = integrate1D l2 u2 
                    (\y->integrate1D l1 u1 
                          (\x->f x y))

zark u v = integrate2D 0.0 u 0.0 v (\x->(\y->x*y))

ints = [1.0..]
zarks = zipWith zark ints (map (2.0*) ints)
rtotals = head zarks : zipWith (+) (tail zarks) rtotals
rtotal n = rtotals!!n

-- BUG: The following line contains a bug:
is = map (^2) ints
-- CORRECT -- is = map (^4) ints

itotals = head is : zipWith (+) (tail is) itotals
itotal n = itotals!!n

es = map (^2) (zipWith (-) rtotals itotals)
etotal n = sum (take n es)

-- The (analytical) result should be zero
-- main = do
-- 	[range] <- getArgs
-- 	putStrLn $ show $ etotal $ read range


