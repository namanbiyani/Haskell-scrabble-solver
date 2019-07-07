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
    | elem a ['A','E','I','O','U','N','R','T','L','S','a','e','i','o','u','n','r','t','l','s'] = 1
    | elem a ['D','G','d','g'] = 2
    | elem a ['B','C','M','P','b','c','m','p'] = 3
    | elem a ['F','H','W','V','Y','f','h','w','v','y'] = 4
    | elem a ['K','k'] = 5
    | elem a ['J','X','j','x'] = 8
    | elem a ['Q','Z','q','z'] = 10
    | otherwise = 0
    
--takes a list of words and sorts them acc to points
sortWords :: [String] -> [String]
sortWords [] = []
sortWords (x:xs) =
    let leftSorted = sortWords [a | a <- xs , (calcScore a) <= (calcScore x)]
        rightSorted = sortWords [a | a <- xs , (calcScore a) > (calcScore x)]
    in leftSorted ++ [x] ++ rightSorted