module Bonus
  (
      module Board_new,
      getScore,
      calcScore,
      pointLst,
  ) where
    
import Board_new

type Coord = (Int,Int)

--Check if letter is on a letter bonus tile
checkLetterDouble :: Coord->Bool
checkLetterDouble (x,y) 
                    | ((x,y)==(3,0) || (x,y)==(11,0) || (x,y)==(0,3) || (x,y)==(0,11) || (x,y)==(6,2) || (x,y)==(8,2) || (x,y)==(2,6) || (x,y)==(2,8) || (x,y)==(3,7) || (x,y)==(7,3) || (x,y)==(12,6) || (x,y)==(6,12) || (x,y)==(11,7) || (x,y)==(7,11) || (x,y)==(12,8) || (x,y)==(8,12) || (x,y)==(6,6) || (x,y)==(8,6) || (x,y)==(6,8) || (x,y)==(8,8)) = True
                    |otherwise = False

checkLetterTriple :: Coord->Bool
checkLetterTriple (x,y)
                    | ((x,y)==(5,1) || (x,y)==(9,1) || (x,y)==(1,5) || (x,y)==(1,9) || (x,y)==(5,9) || (x,y)==(9,5) || (x,y)==(5,5) || (x,y)==(9,9)) = True
                    |otherwise = False



--if letter is on a bonus tile then valued according to it
letterBonus :: Coord->Char->Int
letterBonus (x,y) a
                  | (checkLetterDouble (x,y) ==True) = 2*getScore a
                  | (checkLetterTriple (x,y) ==True) = 3*getScore a
                  | otherwise = getScore a


--Check if a letter of word is on word bonus tile
checkWordDouble :: Coord->Bool
checkWordDouble (x,y) 
                  | ((x,y)==(1,1) || (x,y)==(2,2) || (x,y)==(3,3) || (x,y)==(4,4) || (x,y)==(7,7) || (x,y)==(10,10) || (x,y)==(11,11) || (x,y)==(12,12) || (x,y)==(2,12) || (x,y)==(3,11) || (x,y)==(4,10) || (x,y)==(11,3) || (x,y)==(2,12) || (x,y)==(10,4)) = True
                  |otherwise = False


checkWordTriple :: Coord->Bool
checkWordTriple (x,y) 
                  | ((x,y)==(0,0) || (x,y)==(0,7) || (x,y)==(7,0)) =True
                  | otherwise = False


--if a letter of word is on tile then Word is valued acc to it
wordBonus :: Coord->Char->String->Int
wordBonus (x,y) b xs
                 | (elem True [checkWordDouble (x+i,y)|i<-[0..length (xs)-1]] || elem True [checkWordDouble (x,y+i)|i<-[0..length (xs)-1]]) = 2*calcScore (x,y) b xs
                 |(elem True [checkWordTriple (x+i,y)|i<-[0..length (xs)-1]] || elem True [checkWordTriple (x,y+i)|i<-[0..length (xs)-1]]) = 3*calcScore (x,y) b xs
                 |otherwise = calcScore (x,y) b xs



pointLst :: Coord-> Char->[Char]->[Int]
pointLst (x,y) b xs = if b == 'H'
                        then [ letterBonus (x+i,y) (xs !! i) | i <- [0..length(xs)-1] ] 
                      else if b == 'V'
                        then [ letterBonus (x,y+j) (xs !! j) | j <- [0..length(xs)-1] ] 
                      else [0]

--calculates score
--call this fuction with word(String) as input
calcScore :: Coord->Char->[Char]->Int
calcScore (x,y) b xs = sum(pointLst (x,y) b xs)

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
    let leftSorted = sortWords [a | a <- xs , (calcScore (0,0) 'H' a) <= (calcScore (0,0) 'H' x)]
        rightSorted = sortWords [a | a <- xs , (calcScore (0,0) 'H' a) > (calcScore (0,0) 'H' x)]
    in leftSorted ++ [x] ++ rightSorted
