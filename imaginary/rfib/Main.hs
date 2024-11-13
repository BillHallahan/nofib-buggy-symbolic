import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
    arg <- mkSymbolic
    -- Buggy
    let bb = B.nfib arg

    -- Real
    let rb = R.nfib arg

    assertIO (bb == rb)