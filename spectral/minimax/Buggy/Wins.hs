module Buggy.Wins where

type Win = [[Int]]

wins :: [Win] 
wins = [win1,win2,win3,win4,win5,win6,win7,win8]

win1,win2,win3,win4,win5,win6,win7,win8 :: Win 
win1 = [[1,1,1],
        [0,0,0],
        [0,0,0]]
 
win2 = [[0,0,0],
        [1,1,1],
        [0,0,0]]
 
win3 = [[0,0,0],
        [0,0,0],
        [1,1,1]]
 
win4 = [[1,0,0],
        [1,0,0],
        [1,0,0]]
 
win5 = [[0,1,0],
        [0,1,0],
        [0,1,0]]
 
win6 = [[0,0,1],
        [0,0,1],
        [0,0,1]]
 
win7 = [[1,0,0],
        [0,1,0],
        [0,0,1]]
 
win8 = [[0,0,1],
        [0,1,0],
        [1,0,0]]
