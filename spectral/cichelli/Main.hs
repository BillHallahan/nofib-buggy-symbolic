import qualified Buggy.Prog as B
import qualified Real.Prog as R

import G2.Symbolic

main = do 
 -- Buggy
 let bb = B.cichelli

 -- Real
 let rb = R.cichelli

 assertIO (bb == rb)