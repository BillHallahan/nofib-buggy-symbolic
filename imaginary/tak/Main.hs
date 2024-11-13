import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
    xs <- mkSymbolic  
    ys <- mkSymbolic  
    zs <- mkSymbolic  
    -- Buggy
    let bb = B.tak xs ys zs

    -- Real
    let rb = R.tak xs ys zs

    assertIO (bb == rb)