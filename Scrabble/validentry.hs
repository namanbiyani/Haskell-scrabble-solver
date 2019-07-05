--to check whether the word fits on the board at given coordinates
--1) all points where the word is to be filled should be empty 
--2) length shouldnt exceed board
WordSpace :: Point -> [Point]
if orientation == H
	then 
		WordSpace (x, y) = zip [(x-1) + 1, (x-1) + 2 ..((x-1) + length word)] (repeat y)
		endpt (x,y) xs = (x-1+length xs)

	else 
		WordSpace (x, y) = zip (repeat x) [(y-1) + 1, (y-1) + 2 ..((y-1) + length word]
		endpt (x,y) xs = (y-1+length xs)

if endpt coordinate word <= 12
			then 
				if [(x,ch) | (x,ch) <- initialBoard,x <- Wordspace coordinate, ch /= '*'] /= []
					then putStrLn "These spaces are already filled!"
					else return ()

			else 
				putStrLn "The word does not fit in the board!"		



