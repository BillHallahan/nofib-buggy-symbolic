import qualified Buggy.Prog as B
import qualified Real.Prog as R

import G2.Symbolic

main = do 
 xs <- mkSymbolic
 -- Buggy
 let bb = B.prog xs

 -- Real
 let rb = R.prog xs

 assertIO (bb == rb)