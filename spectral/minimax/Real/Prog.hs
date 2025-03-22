module Real.Prog(prog) where

import Real.Board
import Real.Wins
import Real.Game
import Real.Tree


prog :: String -> String
prog input =
	"OXO\n" ++
	concat (map showMove game)
	where
	board "doesn't happen" = testBoard ++ testBoard
	board _                = testBoard
	game = alternate X max' min' (board input)


testBoard = [[Empty,O,Empty],[Empty,X,Empty],[Empty,Empty,Empty]]

