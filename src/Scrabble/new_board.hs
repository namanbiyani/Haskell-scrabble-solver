import Data.List

--datatype of a board
type Board = [((Integer,Integer),Char)]

rows = 13
columns = 13

--datatype to store a coordinate
type Point = (Int, Int)

--converts board datatype to a string array for easy printing
strBoard :: [(a1, a2)] -> [[[a2]]]
strBoard board = splitEvery rows ([[snd (board !! n)] | n <- [0..168]])

--converts string to board with UI
showBoard :: [((Integer,Integer),Char)] -> String
showBoard  board = let strb = strBoard board
                       rowsep = "\n" ++ (concat $ replicate 13 "+---") ++ "+\n"
                       output = intercalate rowsep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strb
                   in rowsep ++ output ++ rowsep

-- array containing the board coordinate points
boardPoints = [(x,y) | x <- [0..12], y <- [0..12] ]

-- initial board which contains * at all coordinate points
initialBoard = [(x,'*') | x <- boardPoints]

-- function which will print a string into substrings of size = no of columns of the board
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

--function to print board
printBoard :: [((Integer, Integer), Char)] -> IO ()
printBoard board = putStrLn( showBoard board )





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