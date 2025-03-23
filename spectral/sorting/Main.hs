import qualified Buggy.Main as B
import qualified Real.Main as R

import G2.Symbolic

main = do 
    s <- mkSymbolic
    cs <- getContents
 -- Buggy
 let bb = B.mangle s cs

 -- Real
 let rb = R.mangle xs

 assertIO (bb == rb)