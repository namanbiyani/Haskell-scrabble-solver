module Valid_entry
(
   module Board_new,
   isValid,
   check,
)
where

import Board_new

columns = 13

--initialboard !! (columns*x + y) gives (Point,Char) at coordinate (x,y)

-- checks at a point on the board whether that point is empty or has the reqd character
isValid :: [(a, Char)] -> Char -> (Int, Int) -> Bool
isValid board char coord = if snd (board !! (columns*fst(coord) + snd(coord))) == char || snd(board !! (columns*fst  (coord) + snd(coord))) == '*' then True else False

-- call isValid for a list of points
check :: [(a, Char)] -> [(Int, Int)] -> [Char] -> Bool
check board [] [] = True
check board (x:xs) (c:ws) = if (isValid board c x) == False then False else (check board xs ws) 

