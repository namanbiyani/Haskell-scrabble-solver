<<<<<<< HEAD:src/Scrabble/points.hs
=======
--copyright: Guntas Singh Brar...... :-p lol
module Points
  (
      module Board,
      getScore,
      getScore2,
      getScore3,
      getScore4,
      getScore5,
      getScore8,
      getScore10,
      calcScore,
      pointLst,
  ) where
    
import Board

--letters with score 1
getScore :: Char->Int
getScore a = if elem a ['A','E','I','O','U','N','R','T','L','S'] then 1 else getScore2 a

--letters with score 2
getScore2 :: Char->Int
getScore2 a = if elem a ['D','G'] then 2 else getScore3 a

--letters with score 3
getScore3 :: Char->Int
getScore3 a = if elem a ['B','C','M','P'] then 3 else getScore4 a

--letters with score 4
getScore4 :: Char->Int
getScore4 a = if elem a ['F','H','W','V','Y'] then 4 else getScore5 a

--letters with score 5
getScore5 :: Char->Int
getScore5 a = if elem a ['K'] then 5 else getScore8 a

--letters with score 8
getScore8 :: Char->Int
getScore8 a = if elem a ['J','X'] then 8 else getScore10 a

--letters with score 10
getScore10 :: Char->Int
getScore10 a =  if elem a ['Q','Z'] then 10 else 0

>>>>>>> bbe9d4bdac32ae9c2d1afd0b162d34f68dbf6eb9:Scrabble/Points.hs
--returns a list with points of letters
pointLst :: [Char]->[Int]
pointLst x = map (getScore) x

--calculates score
--call this fuction with word(String) as input
calcScore :: [Char]->Int
calcScore x = sum(pointLst x)

--returns score corresponding to every char
getScore :: Char -> Int
getScore a
    | elem a ['A','E','I','O','U','N','R','T','L','S'] = 1
    | elem a ['D','G'] = 2
    | elem a ['B','C','M','P'] = 3
    | elem a ['F','H','W','V','Y'] = 4
    | elem a ['K'] = 5
    | elem a ['J','X'] = 8
    | elem a ['Q','Z'] = 10
    | otherwise = 0
--takes a list of words and sorts them acc to points
sortWords :: [String] -> [String]
sortWords (x:xs) =
    let leftSorted = sortWords [a | a <- xs , (calcScore a) <= (calcScore x)]
        rightSorted = sortWords [a | a <- xs , (calcScore a) > (calcScore x)]
    in leftSorted ++ [x] ++ rightSorted