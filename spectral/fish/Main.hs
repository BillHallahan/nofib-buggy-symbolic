import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
    a1 <- mkSymbolic
    a2 <- mkSymbolic
    a3 <- mkSymbolic
    -- Buggy
    let bb = B.fmt (B.pseudolimit a1 a2 a3)

    -- Real
    let rb = R.fmt (R.pseudolimit a1 a2 a3)

    assertIO (bb == rb)