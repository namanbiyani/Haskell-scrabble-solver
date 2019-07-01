module Change_Board (
   module Scrabble.Board, 
   prevBoard,
   nexBoard,
   newBoard,
   toInt,
   addBoard,
) where
import Scrabble.Board
import Data.List

--a***********************NEW BOARD FUNCTI0NS BELOW**************************

--to get the board elements list before the element to be changed 
prevBoard :: ((Integer,Integer),Char) -> [((Integer,Integer),Char)]
prevBoard ((x,y),a) = take (toInt(x*13 + y)) initialBoard

--to get the board elements list after the element to be changed 
nexBoard :: ((Integer,Integer),Char) -> [((Integer,Integer),Char)]
nexBoard ((x,y),a) = drop (toInt(x*13 + y + 1)) initialBoard

--typecasting (sort of) as the take and drop functions used take Int as input
toInt :: Integer -> Int
toInt a = read (show a) :: Int

--concat prevBoard and nexBoard
newBoard :: ((Integer,Integer),Char) -> [((Integer,Integer),Char)] 
newBoard ((x,y),a) = prevBoard ((x,y),a) ++ [((x,y),a)] ++ nexBoard ((x,y),a) 

addBoard word = foldr (\x acc -> newBoard x) initialBoard