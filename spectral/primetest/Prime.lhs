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
--  This program contains a bug of tipe 3:./Prime.lhs  --
--  	Irrefutable pattern failed for pattern (t:ts)	 --
---------------------------------------------------------

\section{Probabalistic Primality Testing}
%$Log: Prime.lhs,v $
%Revision 1.3  2004/11/30 15:25:33  simonmar
%The compiler now detects local modules that overlap with package
%modules, so we must rename Random to avoid the clash.
%
%Revision 1.2  1996/07/25 21:32:57  partain
%Bulk of final changes for 2.01
%
%Revision 1.1  1996/01/08 20:04:21  partain
%Initial revision
%
%Revision 1.2  92/06/30  18:34:22  dlester
%Bug fix; now works for 1.
%
%Revision 1.1  92/06/30  15:55:20  dlester
%Initial revision
%

> module Prime (multiTest) where
> import IntLib
> import MyRandom

The function @multiTest@ tests the integer @n@ for primality @k@ times
using the random numbers supplied in @rs@; it returns a @Bool@ and a
depleted sequence of random numbers. When the @Bool@ is @True@ the
number is prime with probability $1 - 4^{-@k@}$; the number is
definitely composite when the @Bool@ is @False@.

> multiTest :: Int -> [Int] -> Integer -> (Bool, [Int])
> multiTest k rs n
>  = if n <= 1 || even n then (n==2, rs) else mTest k rs
>    where mTest 0 rs = (True, rs)
>          mTest k rs = if t then mTest (k-1) rs' else (False, rs')
>                           where (t, rs') = singleTest n (findKQ n) rs

The function @findKQ@ takes an odd integer $n$ and returns the tuple
$(k,q)$ such that $n = q2^k+1$.

> findKQ :: Integer -> (Integer, Integer)
> findKQ n = f (0, (n-1))
>		-- BUG: The following line contains a bug
>            where f (k,q) = if r == 0 then f (k, d) else (k, q)
>		-- CORRECT -- then f (k+1, d) else (k, q)
>                            where (d, r) = q `divMod` 2

The function @singleTest@ takes an odd, positive, @Integer@ @n@ and a
pair of @Integer@'s derived from @n@ by the @findKQ@ function

> singleTest :: Integer -> (Integer, Integer) -> [Int] -> (Bool, [Int])
> singleTest n kq rs
>  = (singleTestX n kq (2+x), rs')
>    where (x, rs')       = random (n-2) rs

Given a value @x@ we can test whether we have a witness to @n@'s
compositeness using @singleTestX@.

> singleTestX n (k, q) x
>  = t == 1 || t == n-1 || witness ts
>    where (t:ts)         = take (fromInteger k) (iterate square (powerMod x q n))
>          witness []     = False
>          witness (t:ts) = if t == n-1 then True       else
>                           if t == 1   then False      else
>                                            witness ts
>          square x       = (x*x) `mod` n

The @random@ function takes a number @n@ and a list of pseudo-random
numbers @rs@ and returns a tuple consisting of an @Integer@ $x$ in the
range $0 \leq x < @n@$, and the remainder of the pseudo-random
numbers.

> random :: Integer -> [Int] -> (Integer, [Int])
> random n rs = (makeNumber 65536 (uniform ns rs1), rs2)
>               where ns        = chop 65536 n
>                     (rs1,rs2) = splitAt (length ns) rs

The @uniform@ function generates a sequence of @Integer@'s such that,
when considered as a sequence of digits, we generate a number uniform
in the range @0..ns@ from the random numbers @rs@.

> uniform :: [Integer] -> [Int] -> [Integer]
> uniform [n]    [r]    = [toInteger r `mod` n]
> uniform (n:ns) (r:rs) = if t == n then t: uniform ns rs
>                                   else t: map ((`mod` 65536). toInteger) rs
>                         where t  = toInteger r `mod` (n+1)

This concludes the Prime testing algorithm.
