-- 
--      Patricia Fasel
--      Los Alamos National Laboratory
--      1990 August
--
import	Pic
import  PicType	-- added by partain
import IO(hPutStr,stderr)--1.3

main = do
    hPutStr stderr "Enter the number of particles: "
    s <- getContents
    let (nPart, rest) = (head (reads s)) :: (Int, String)
    putStrLn (takeWhile ((/=) '\n') s ++ (pic nPart))

