import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
 arg <- mkSymbolic
 let n = (arg)::Int
 
 -- Buggy
 let bb = B.bernoulli n

 -- Real
 let rb = R.bernoulli n

 assertIO (bb == rb)