import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

main = do
 digits <- mkSymbolic
 
 -- Buggy
 let bb = take digits B.e

 -- Real
 let rb = take digits R.e

 assertIO (bb == rb)