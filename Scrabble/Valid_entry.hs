module Valid_entry
(
	module Board_new,
	isValid,
)		
where

import Board_new

columns = 13

--initialboard !! (columns*x + y) gives (Point,Char) at coordinate (x,y)

isValid board char coord = if snd (board !! (columns*fst(coord) + snd(coord))) == char || snd(board !! (columns*fst  (coord) + snd(coord))) == '*' then True else False

check board [] [] = True
check board (x:xs) (c:ws) = if (isValid board c x) == True then True else (check board xs ws) 

listOfPoints ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]] 