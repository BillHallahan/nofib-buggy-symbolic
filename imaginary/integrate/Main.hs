import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
    range <- mkSymbolic
 
    -- Buggy
    let bb = B.etotal range

    -- Real
    let rb = R.etotal range

    assertIO (bb == rb)