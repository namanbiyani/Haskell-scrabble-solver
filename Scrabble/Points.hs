module Points
  (
      module Board_new,
      getScore,
      calcScore,
      pointLst,
      sortWords,
  ) where
    
import Board_new

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
sortWords [] = []
sortWords (x:xs) =
    let leftSorted = sortWords [a | a <- xs , (calcScore a) <= (calcScore x)]
        rightSorted = sortWords [a | a <- xs , (calcScore a) > (calcScore x)]
    in leftSorted ++ [x] ++ rightSorted