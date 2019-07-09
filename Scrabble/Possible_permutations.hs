module Possible_permutations
(
    module Board_new,
    findAllPermutations,
    findEmptyPoints,
    findNonEmptyPoints,
    firstList,
    getACombin,
    genPossibleFilling,
    delete',
    permutations',
    listOfP,
    listOfPoint,
    findAllWords,
    formWord,
)
where
    
import Data.List
import Board_new

--This function finds the points that are not filled and returns a list of them
findEmptyPoints :: [((Int,Int),Char)]->[((Int,Int),Char)]
findEmptyPoints pointAndStatus = [(point,status)|(point,status) <- pointAndStatus, status == '*']     --Checked

--This function finds and returns a list of all the points that are filled
findNonEmptyPoints :: [((Int,Int),Char)]->[((Int,Int),Char)]
findNonEmptyPoints pointAndStatus = [(point,status)|(point, status)<- pointAndStatus, status /= '*']  --Checked

--This takes a list with elements (points, char) and returns a list containing points
firstList :: [((Int,Int),Char)] -> [(Int,Int)]
firstList [] = []
firstList xs = [fst (head xs)] ++ firstList (tail xs)

-- This function takes a list of points which are not yet filled and list of possible characters to be filled and returns a
-- possible combination of filled points
-- getACombin :: [((Int,Int),Char)] -> [Char] -> [((Int,Int),Char)]
getACombin emptyPointList x = zip (firstList emptyPointList) x 

-- This function takes a list of points and a list of possible characters to fill and generate all possible combinations of them
--filled
-- genPossibleFilling :: [((Int, Int), Char)] -> [Char] -> [[((Int, Int), Char)]]
genPossibleFilling emptyPointList listOfPossChars = [getACombin emptyPointList x|x<- permutations' listOfPossChars]

-- We need to pass array of points and corresponding characters filled there and a list of
--possible alphabets that can be filled. It filters points which are empty and returns list of all possible combinations in which
-- empty points can be filled.
-- findAllPermutations :: [((Int, Int), Char)] -> [Char] -> [[((Int, Int), Char)]]
findAllPermutations pointAndStatus listOfPossChars = do
                                                     let xs = findEmptyPoints pointAndStatus
                                                     let ys = findNonEmptyPoints pointAndStatus
                                                     let possFills = genPossibleFilling xs listOfPossChars
                                                     [x ++ ys| x<- possFills] ++ [ys ++ x| x<-possFills]

-- this Function is to be called from Scrabble.hs , it takes board and letters and gives the possible permutations
findAllWords point_status letters = [word | word <- [formWord x | x <- (findAllPermutations point_status letters) ]]

-- this function forms words from list of (point,char)
formWord list_point_char = [snd x | x <- list_point_char]

delete' :: Eq a => a -> [a] -> [a]
delete' x xs = [y | y <- xs , y /= x]

-- permutation of string
permutations' :: Eq a => [a] -> [[a]]
permutations' xs = if length xs == 0 then [[]] else [x:ys | x <- xs, ys <- permutations' $ delete' x xs]

-- takes a tuple and gives a list of (point,char)
listOfP board ((a,b),(c,d)) = [board !! (13*x + y) | (x,y) <- listOfPoint ((a,b),(c,d))]

-- takes a tuple and gives a list of points
listOfPoint ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]]