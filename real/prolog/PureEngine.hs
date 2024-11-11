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
--  This program contains a bug of tipe 2              --
---------------------------------------------------------

--
-- The Pure Prolog inference engine (using explicit prooftrees)
-- Mark P. Jones November 1990
--
-- uses Haskell B. version 0.99.3
--
module Engine(prove) where

import PrologData
import Subst

--- Calculation of solutions:

-- Each node in a prooftree corresponds to:
-- either: a solution to the current goal, represented by Done s, where s
--         is the required substitution
-- or:     a choice between a number of subtrees ts, each corresponding to a
--         proof of a subgoal of the current goal, represented by Choice ts.
--         The proof tree corresponding to an unsolvable goal is Choice [] 

data Prooftree = Done Subst  |  Choice [Prooftree]

-- prooftree uses the rules of Prolog to construct a suitable proof tree for
--           a specified goal
prooftree   :: Database -> Int -> Subst -> [Term] -> Prooftree
prooftree db = pt
 where pt           :: Int -> Subst -> [Term] -> Prooftree
       pt n s []     = Done s
	-- BUG:The following line contains a bug
       pt n s (g:gs) = Choice [ pt (n) (u@@s) (map (apply u) (tp++gs))
	-- CORRECT -- pt n s (g:gs) = Choice [ pt (n+1) (u@@s) (map (apply u) (tp++gs))
                              | (tm:==tp)<-renClauses db n g, u<-unify g tm ]

-- search performs a depth-first search of a proof tree, producing the list
--        of solution substitutions as they are encountered.
search              :: Prooftree -> [Subst]
search (Done s)      = [s]
search (Choice pts)  = [ s | pt <- pts, s <- search pt ]

prove    :: Database -> [Term] -> [Subst]
prove db  = search . prooftree db 1 nullSubst

--- End of PureEngine.hs
