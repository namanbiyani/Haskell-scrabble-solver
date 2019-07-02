module Possible_permutations
(
    module Board_new,
    findAllPermutations,
    findEmptyPoints,
    findNonEmptyPoints,
    firstList,
    getACombin,
    genPossibleFilling,
)
where

import Data.List
import Board_new


--This function finds the points that are not filled and returns a list of them
findEmptyPoints:: [((Int,Int),Char)]->[((Int,Int),Char)]
findEmptyPoints pointAndStatus = [(point,status)|(point, status)<-pointAndStatus, status == '*']     --Checked

--This function finds and returns a list of all the points that are filled
findNonEmptyPoints:: [((Int,Int),Char)]->[((Int,Int),Char)]
findNonEmptyPoints pointAndStatus = [(point,status)|(point, status)<- pointAndStatus, status /= '*']  --Checked

--This takes a list with elements (points, char) and returns a list containing points
firstList [] = []
firstList xs = [fst (head xs)] ++ firstList (tail xs)

-- This function takes a list of points which are not yet filled and list of possible characters to be filled and returns a
-- possible combination of filled points
getACombin emptyPointList x = zip (firstList emptyPointList) x 

-- This function takes a list of points and a list of possible characters to fill and generate all possible combinations of them
--filled
genPossibleFilling emptyPointList listOfPossChars = [getACombin emptyPointList x|x<- permutations listOfPossChars]

--This is the main function to be called. We need to pass array of points and corresponding characters filled there and a list of
--possible alphabets that can be filled. It filters points which are empty and returns list of all possible combinations in which
-- empty points can be filled.
findAllPermutations pointAndStatus listOfPossChars = do
                                                     let xs = findEmptyPoints pointAndStatus
                                                     let ys = findNonEmptyPoints pointAndStatus
                                                     let possFills = genPossibleFilling xs listOfPossChars
                                                     map (++ ys) possFills

                                                     