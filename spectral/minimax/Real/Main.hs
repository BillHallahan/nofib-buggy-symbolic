module Main where

import Prog (prog)

-- #ifdef PAR
-- main input = prog input
-- #else
-- suspect:main ~((Str str):_) = [ReadChan stdin, AppendChan stdout (prog str)]
main = do
	_ <- getContents
	putStr (prog "")
-- #endif
