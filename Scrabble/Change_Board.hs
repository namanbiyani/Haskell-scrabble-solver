
module Change_Board (
   module Board_new, 
   prevBoard,
   nexBoard,
   newBoard,
   toInt,
   inputBoard,
   putWordAcrs,
   putWordDown,
   mkBoardStrA,
   mkBoardStrD,
   pointListAcrs,
   pointListDwn,
) where
import Board_new
import Data.List

--[*****************************INPUT A LETTER IN THE BOARD*****************************]

--to get the board elements list before the element to be changed 
prevBoard :: (Int,Int) -> [((Int,Int),Char)] -> [((Int,Int),Char)]
prevBoard (rw,clm) board = take (toInt(rw*13 + clm)) board

--to get the board elements list after the element to be changed 
nexBoard :: (Int,Int) -> [((Int,Int),Char)] -> [((Int,Int),Char)]
nexBoard (rw,clm) board = drop (toInt(rw*13 + clm + 1)) board

--typecasting (sort of) as the take and drop functions used take Int as input
toInt :: Int -> Int
toInt a = read (show a) :: Int

--concat prevBoard and nexBoard
newBoard :: ((Int,Int),Char) -> [((Int,Int),Char)] -> [((Int,Int),Char)] 
newBoard ((rw,clm),a) board = (prevBoard (rw,clm) board) ++ [((rw,clm),a)] ++ (nexBoard (rw,clm) board)


--[*****************************INPUT WORDS ACROSS THE BOARD*****************************]


--return a list of coordinates (rw,clm)
pointListAcrs :: (Int,Int,Int) -> [(Int,Int)]
pointListAcrs (rw,clm1,clm2) = zip (take (toInt((clm2 - clm1) + 1)) (repeat rw)) ([clm1..clm2])

--zips the coordinates list with the word
mkBoardStrA :: (Int,Int,Int) -> [Char] -> [((Int,Int),Char)]
mkBoardStrA (rw,clm1,clm2) a = zip (pointListAcrs (rw,clm1,clm2)) a

--concat prevBoard , input and nexBoard
putWordAcrs :: (Int,Int,Int) -> [Char] -> [((Int,Int),Char)] -> [((Int,Int),Char)]
putWordAcrs (rw,clm1,clm2) a board = (prevBoard (rw,clm1) board) ++ (mkBoardStrA (rw,clm1,clm2) a) ++ (nexBoard (rw,clm2) board)


--[*****************************INPUT WORDS DOWN THE BOARD*****************************]


--return a list of coordinates (rw,clm)
pointListDwn :: (Int,Int,Int) -> [(Int,Int)]
pointListDwn (rw1,rw2,clm) = zip ([rw1..rw2]) (take (toInt((rw2 - rw1) + 1)) (repeat clm))

--zips the coordinates list with the word
mkBoardStrD :: (Int,Int,Int) -> [Char] -> [((Int,Int),Char)]
mkBoardStrD (rw1,rw2,clm) a = zip (pointListDwn (rw1,rw2,clm)) a

--Adds letters recursively to the board
inputBoard :: [((Int,Int),Char)] -> [((Int,Int),Char)] -> [((Int,Int),Char)]
inputBoard (x:xs) board = if xs == [] then newBoard x board else inputBoard xs (newBoard x board)

--add words down the board
putWordDown :: (Int,Int,Int) -> [Char] -> [((Int,Int),Char)] -> [((Int,Int),Char)]
putWordDown (rw1,rw2,clm) a board = inputBoard (mkBoardStrD (rw1,rw2,clm) a) board

