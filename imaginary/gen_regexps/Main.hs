import qualified MainBuggy as B
import qualified MainReal as R

import G2.Symbolic

interact2 :: (String -> String) -> (String -> String) -> IO ()
interact2 f g = do
    s <- getContents
    let x = f s
        y = g s
    assertIO (x == y)
    putStr x

main = do
 interact2 (("Enter a generator: " ++).show.B.numchars.B.expand.head.lines) (("Enter a generator: " ++).show.R.numchars.R.expand.head.lines)
 
