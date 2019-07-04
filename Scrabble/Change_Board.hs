
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

--[*****************************INPUT A LETTER IN THE BOARD*****************************]

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


--[*****************************INPUT WORDS ACROSS THE BOARD*****************************]


--return a list of coordinates (rw,clm)
pointListAcrs :: (Integer,Integer,Integer) -> [(Integer,Integer)]
pointListAcrs (rw,clm1,clm2) = zip (take (toInt((clm2 - clm1) + 1)) (repeat rw)) ([clm1..clm2])

--zips the coordinates list with the word
mkBoardStrA :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)]
mkBoardStrA (rw,clm1,clm2) a = zip (pointListAcrs (rw,clm1,clm2)) a

--concat prevBoard , input and nexBoard
putWordAcrs :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
putWordAcrs (rw,clm1,clm2) a board = (prevBoard (rw,clm1) board) ++ (mkBoardStrA (rw,clm1,clm2) a) ++ (nexBoard (rw,clm2) board)


--[*****************************INPUT WORDS DOWN THE BOARD*****************************]


--return a list of coordinates (rw,clm)
pointListDwn :: (Integer,Integer,Integer) -> [(Integer,Integer)]
pointListDwn (rw1,rw2,clm) = zip ([rw1..rw2]) (take (toInt((rw2 - rw1) + 1)) (repeat clm))

--zips the coordinates list with the word
mkBoardStrD :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)]
mkBoardStrD (rw1,rw2,clm) a = zip (pointListDwn (rw1,rw2,clm)) a

--Adds letters recursively to the board
inputBoard :: [((Integer,Integer),Char)] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
inputBoard (x:xs) board = if xs == [] then newBoard x board else inputBoard xs (newBoard x board)

--add words down the board
putWordDown :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
putWordDown (rw1,rw2,clm) a board = inputBoard (mkBoardStrD (rw1,rw2,clm) a) board

