import qualified MainBuggy as B
import qualified MainReal as R

import Data.Complex

import G2.Symbolic

main = do
    arg <- mkSymbolic
-- 	print (round (realPart (sum [f n | n <- [1 .. (read arg)]])))
    -- Buggy
    let bb = round (realPart (sum [B.f n | n <- [1 .. (arg)]]))

    -- Real
    let rb = round (realPart (sum [R.f n | n <- [1 .. (arg)]]))

    assertIO (bb == rb)