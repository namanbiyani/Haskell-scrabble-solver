module Filter
(
    module Board_new,
    module Possible_permutations,
    tuplePoints,
    firstList,
    secondList,
    filter,
    filterPointsList,
)
where

import Possible_permutations --findEmptyPoints, findNonEmptyPoints    
import Board_new
import Data.List
import Data.Char
import Data.Map

--function that will give a list of points between (a,c) and (b,d) 
tuplePoints ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]] 

-- function to convert tuplePoints to (Point,Char) list . Use :
--initialboard !! (columns*x + y) gives (Point,Char) at coordinate (x,y)

--This takes a list with elements (points, char) and returns a list containing points
firstList [] = []
firstList xs = [fst (head xs)] ++ firstList (tail xs)

--This takes a list with elements (points, char) and returns a list containing char
secondList [] = []
secondList xs = [snd (head xs)] ++ firstList (tail xs)

filter tuples board = 

filterPointsList pointList
  | length (findEmptyPoints pointList) >= 7 = False
  | (length findNonEmptyPoints) == (length pointList) = False 
  | otherwise = True

