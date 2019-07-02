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
prevBoard :: (Integer,Integer) -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
prevBoard (rw,clm) board = take (toInt(rw*13 + clm)) board

--to get the board elements list after the element to be changed 
nexBoard :: (Integer,Integer) -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
nexBoard (rw,clm) board = drop (toInt(rw*13 + clm + 1)) board

--typecasting (sort of) as the take and drop functions used take Int as input
toInt :: Integer -> Int
toInt a = read (show a) :: Int

--concat prevBoard and nexBoard
newBoard :: ((Integer,Integer),Char) -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)] 
newBoard ((rw,clm),a) board = (prevBoard (rw,clm) board) ++ [((rw,clm),a)] ++ (nexBoard (rw,clm) board)


--a****************************INPUT WORDS IN THE BOARD ACROSS******************************

--concat prevBoard , input and nexBoard
putWordAcrs :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
putWordAcrs (rw,clm1,clm2) a board = (prevBoard (rw,clm1) board) ++ (mkBoardStr (rw,clm1,clm2) a) ++ (nexBoard (rw,clm2) board)

--return a list of coordinates (rw,clm)
pointListAcrs :: (Integer,Integer,Integer) -> [(Integer,Integer)]
pointListAcrs (rw,clm1,clm2) = zip (take (toInt((clm2 - clm1) + 1)) (repeat rw)) ([clm1..clm2])

--zips the coordinates list with the word
mkBoardStr :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)]
mkBoardStr (rw,clm1,clm2) a = zip (pointListAcrs (rw,clm1,clm2)) a