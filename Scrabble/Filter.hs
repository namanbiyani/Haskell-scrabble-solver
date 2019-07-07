module Filter 
(
    module Board_new,
    genPointsfromTuple,
    noOfEmptyCell,
    isValid,
    findPossTuples,
)
where

import Board_new

type Point = (Int,Int)

--The tuple contains two points which are the ending points
genPointsfromTuple :: (Ord b, Ord a, Num b, Num a) => [(a, b)] -> [(a, b)]
genPointsfromTuple [(x,y),(a,b)] = if (x == a && y == b) then [(x,y)]
                                   
                                   else if (x == a)
                                            then do
                                                    if y<b then [(x,y)] ++ genPointsfromTuple [(x,y+1),(a,b)] else [(a,b)] ++ genPointsfromTuple [(a,b+1),(x,y)]
                                  
                                   else
                                         do
                                            if x<a then [(x,y)] ++ genPointsfromTuple [(x+1,y),(a,b)] else  [(a,b)] ++ genPointsfromTuple [(a+1,b),(x,y)]

--Gives the no of empty points in the tuples
noofEmptyCell :: 
noOfEmptyCell [] scrabbleBoard column = 0
noOfEmptyCell (x:xs) scrabbleBoard column = if scrabbleBoard !! (column*fst(x) + snd(x)) == '*' then (noOfEmptyCell xs scrabbleBoard column + 1) else noOfEmptyCell xs scrabbleBoard column

--This function evaluates whether the given tuple is valid or not
isValid a count = if count > 7 then False
                  else if count == 0 then False
                  else if count == length a then False 
                  else True

-- This function is main and should be called with the scrabble board and list of tuples of form [(a,b),(x,y)] and no of columns and it returns a list of the valid tuples by checkomg against scrabble board.
findPossTuples scrabbleBoard [] column = []
findPossTuples scrabbleBoard xs column = do
                                         let ys = map (genPointsfromTuple) xs
                                         [y| y <- ys, isValid y (noOfEmptyCell y scrabbleBoard column)]