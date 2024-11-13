import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
    arg <- mkSymbolic
    -- Buggy
    let bb = B.primes !! arg

    -- Real
    let rb = R.primes !! arg

    assertIO (bb == rb)