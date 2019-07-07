 module Board_new
   (
       initialBoard,
       Board,
       Point,
       aboveP,
       belowP,
       leftOfP,
       rightOfP,
       allAboveP,
       allBelowP,
       allLeftOfP,
       allRightOfP,
       neighbors4P,
       strBoard,
       boardPoints,
       showBoard,
       splitEvery,
       printBoard
   ) where
import Data.List

--datatype of a board
type Board = [((Int,Int),Char)]

rows = 13
columns = 13

--datatype to store a coordinate
type Point = (Int, Int)

-- return the point directly above the given point
aboveP :: Point -> Point
aboveP   (x, y) = (x, y - 1)

-- | return the point directly below the given point
belowP :: Point -> Point
belowP   (x, y) = (x, y + 1)

-- return the point directly above the given point
leftOfP :: Point -> Point
leftOfP  (x, y) = (x - 1, y)

-- return the point directly above the given point
rightOfP :: Point -> Point
rightOfP (x, y) = (x + 1, y)


-- returns all points above p 
allAboveP :: Point -> [Point]
allAboveP (x, y) = zip (repeat x) [y - 1, y - 2 ..0]

-- returns all points below p
allBelowP :: Point -> [Point]
allBelowP (x, y) = zip (repeat x) [y + 1, y + 2 ..12]

-- returns all points to the right of p
allRightOfP :: Point -> [Point]
allRightOfP (x, y) = zip [x + 1, x + 2 ..12] (repeat y)

-- returns all points to the left of p
allLeftOfP :: Point -> [Point]
allLeftOfP (x, y) = zip [x - 1, x - 2 .. 0 ] (repeat y)

-- returns all neighbours of p
neighbors4P :: Point -> [Point]
neighbors4P p = [aboveP p, belowP p, leftOfP p, rightOfP p]

--converts board datatype to a string array for easy printing
strBoard :: [(a1, a2)] -> [[[a2]]]
strBoard board = splitEvery rows ([[snd (board !! n)] | n <- [0..168]])

--converts string to board with UI
showBoard :: [((Int,Int),Char)] -> String
showBoard  board = let strb = strBoard board
                       rowsep = "\n" ++ (concat $ replicate 13 "+---") ++ "+\n"
                       output = intercalate rowsep $ map (\x ->"| " ++ (intercalate " | " x) ++ " |") strb
                   in rowsep ++ output ++ rowsep

-- array containing the board coordinate points
boardPoints = [(x,y) | x <- [0..12], y <- [0..12] ]

-- initial board which contains * at all coordinate points
initialBoard :: [((Int, Int), Char)]
initialBoard = [(x,'*') | x <- boardPoints]

-- function which will print a string into substrings of size = no of columns of the board
splitEvery :: Int -> [a] -> [[a]]
splitEvery n = takeWhile (not.null) . map (take n) . iterate (drop n)

--function to print board
printBoard :: [((Int, Int), Char)] -> IO ()
printBoard board = putStrLn( showBoard board )