module Scrabble (
    module Points,
    module Possible_permutations,
    module DictSearch,
    module Change_Board, 
    module Board_new,
    module Valid_entry,
    module Filter,
 --    module Filter,
    randomChar,
    input,
    game2Player,
    gameWithComputer,
    listOfTuplesH,
    listOfTuplesV,
 ) where
 
 import Valid_entry
 import Filter
 import Board_new
 import Points
 import Possible_permutations
 import DictSearch
 import Change_Board
 import System.Random
 import Data.List
 import System.IO
 import System.IO.Unsafe
 import Data.Char
 import Data.Typeable
 -- import Bonus
 
 --putWordAcrs function , (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
 --putWordDown :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
 
 randomChar :: Int->Char
 randomChar x =  ['a'..'z'] !! (mod x 26) 
 
 -- listOfPoints ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]] 
 
 main = do   
     putStrLn "****************SCRABBLE*******************"
     putStrLn "Enter 1 to play 2 player game"
     putStrLn "Enter 2 to play with  me"
     line <- getLine  
     if null line  
         then return ()  
         else if line == "1"
                 then do
                         let sa = 0
                         let sb = 0
                         let t = 0   
                         let board = initialBoard
                         game2Player $ board sa sb t
                         return ()
                 else
                      if line == "2"
                         then do gameWithComputer initialBoard
                                 return ()
                         
                      else
                         do
                           putStrLn "wrong choice"
                           main
                           return()

 
--  game2Player :: [((Int, Int), Char)] -> IO ()
 game2Player initialBoard scoreA scoreB turn = do
         putStrLn "Enter 1 to display Board"
         putStrLn "Enter 2 to add a word to the existing Board"
         putStrLn "Enter 3 to start game "
         line <- getLine
         if null line
             then return ()
             else if line == "1"
                 then do 
                         printBoard initialBoard 
                         game2Player $ initialBoard scoreA scoreB turn
                         return ()
                 else
                      if line == "2"
                         then do
                              putStrLn "Enter word to be added to the board"
                              word <- getLine
                              putStrLn "Enter coordinate of the starting index of the word ( (0,0) denotes \nthe top left box and (12,12) denotes the bottom right box"
                              coordinate' <- getLine
                              let coordinate = read coordinate' :: (Int,Int)
                              putStrLn "Enter orientation (H for horizontal and V for vertical)"
                              orientation <- getLine
                             --  let orientation = read orientation' :: Char
                              
                              -- check if word addition is possible there
                              -- check if word is correct
                              if orientation == "H"
                                 then do
                                     --Checks if the word is in dictionary
                                     if (snd(coordinate) + length(word)) > 12 
                                        then do
                                            putStrLn "Word cannot be fitted in the board"
                                            game2Player $ initialBoard scoreA scoreB turn
                                            return ()
                                        else do
                                            putStrLn "Word can be fitted in the board"
                                            return ()
                                        
                                     if search word == True
                                         then do
                                             putStrLn "Right word"
                                             return ()
                                         else do
                                             putStrLn "word not found in dictionary"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
 
                                     --Checks if the new word added is overwriting the board
                                     if check initialBoard (listOfPoints (coordinate,(fst(coordinate),snd(coordinate) + length (word) -1))) word == True
                                         then do 
                                             putStrLn "right"
                                             return ()
                                         else do
                                             putStrLn "Wrong addition of word"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
                                             return ()
 
                                     putStrLn "Modified Board is .............."
                                     printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                     --return ()
                                     game2Player $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                     return ()
                                 else do
                                     if (fst(coordinate) + length(word)) > 12 
                                         then do
                                             putStrLn "Word cannot be fitted in the board"
                                             game2Player $ initialBoard scoreA scoreB turn
                                             return ()
                                         else do
                                             putStrLn "Word can be fitted in the board"
                                             return ()
                                        
                                     --Checks if the word is in dictionary
                                     if search word == True
                                         then do
                                             putStrLn "Right word"
                                             return ()
                                         else do
                                             putStrLn "Word not found in dictionary"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
 
                                     --Checks if the new word added is overwriting the board
                                     if check initialBoard (listOfPoints (coordinate,(fst(coordinate)  + length (word) -1,snd(coordinate)))) word == True
                                         then do 
                                             putStrLn "right"
                                             return ()
                                         else do
                                             putStrLn "Wrong addition of word"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
                                             return ()
                                     putStrLn "Modified Board is .............."
                                     printBoard $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                     --return ()
                                     game2Player $ ((putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard) scoreA scoreB turn)
                                     return () 
                              return () 
                         else
                             do 
                                -- let a = 
                                let sa = "Score A = " ++ show(scoreA)
                                let sb = "Score B = " ++ show(scoreB)
                                print sa
                                print sb
                                if turn == 0 then print "Turn of player A" else print "Turn of player B"

                                putStrLn "Form a word using the table and the following words"
                                printBoard initialBoard
                             --    let input' = input !! 0
                                putStrLn (input !! 0)
                                putStrLn "Enter your word : "
                                word <- getLine
                                putStrLn "Enter coordinate of the starting index of the word ( (0,0) denotes \nthe top left box and (12,12) denotes the bottom right box"
                                coordinate' <- getLine
                                let coordinate = read coordinate' :: (Int,Int)
                                putStrLn "Enter orientation (H for horizontal and V for vertical)"
                                orientation <- getLine
                             --    let orientation = read orientation' :: Char
                                -- check if word addition is possible 
                                -- check if word is valid from DictSearch
                                let score = calcScore word
                                let msg = "Score for the word " ++ show(score) ++ " ! "
                                putStrLn msg
                                if orientation == "H"
                                 then do
                                     if (snd(coordinate) + length(word)) > 12 
                                         then do
                                             putStrLn "Word cannot be fitted in the board"
                                             game2Player $ initialBoard scoreA scoreB turn
                                             return ()
                                         else do
                                             putStrLn "Word can be fitted in the board"
                                             return ()
                                        
                                     --Checks if the word is in dictionary
                                     if search word == True
                                         then do
                                             putStrLn "Right word"
                                             return ()
                                         else do
                                             putStrLn "word not found in dictionary"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
 
                                     --Checks if the new word added is overwriting the board
                                     if check initialBoard (listOfPoints (coordinate,(fst(coordinate),snd(coordinate) + length (word) -1))) word == True
                                         then do 
                                             putStrLn "right"
                                             return ()
                                         else do
                                             putStrLn "Wrong addition of word"
                                             printBoard initialBoard
                                             game2Player $ initialBoard scoreA scoreB turn
                                             return ()
                                     
                                     putStrLn "Modified Board is .............."
                                     printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                     
                                     -- score calculation
                                     let score = (show ( calcScore word)) ++ "is the score"
                                     putStrLn score
                                     if turn == 0 
                                        then do
                                            let a = score + scoreA
                                            let b = scoreB
                                            let t = 1
                                            print "New Score for A is " ++ show(a)
                                            game2Player $ ((putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard)) a b t
                                            return ()
                                        else do
                                            let a = scoreA
                                            let b = score + scoreB
                                            let t = 0
                                            print "New Score for B is " ++ show(b)
                                            game2Player $ ((putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1)) word initialBoard) a b t
                                            return ()                                             
                                 
                                     return ()
                                 else do
                                     if (snd(coordinate) + length(word)) > 12 
                                         then do
                                             putStrLn "Word cannot be fitted in the board"
                                             game2Player initialBoard
                                             return ()
                                         else do
                                             putStrLn "Word can be fitted in the board"
                                             return ()
                                         
                                     --Checks if the word is in dictionary
                                     if search word == True
                                         then do
                                             putStrLn "Right word"
                                             return ()
                                         else do
                                             putStrLn "word not found in dictionary"
                                             printBoard initialBoard
                                             game2Player initialBoard
 
                                     --Checks if the new word added is overwriting the board
                                     if check initialBoard (listOfPoints (coordinate,(fst(coordinate)  + length (word) -1,snd(coordinate)))) word == True
                                         then do 
                                             putStrLn "right"
                                             return ()
                                         else do
                                             putStrLn "Wrong addition of word"
                                             printBoard initialBoard
                                             game2Player initialBoard
                                             return ()
                                     putStrLn "Modified Board is .............."
                                     printBoard $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                     -- return () 

                                     -- score calculation
                                     let score = (show ( calcScore word)) ++ "is the score"
                                     putStrLn score

                                     if turn == 0 
                                        then do
                                            let a = score + scoreA
                                            let b = scoreB
                                            let t = 1
                                            print "New Score for A is " ++ show(a)
                                            game2Player $ ((putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard) a b t)
                                            return ()
                                        else do
                                            let a = scoreA
                                            let b = score + scoreB
                                            let t = 0
                                            print "New Score for B is " ++ show(b)
                                            game2Player $ ((putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard) a b t)
                                            return ()     
                                return ()
 
 listOfPoints ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]] 
 
--  strToPrint :: [[String]] -> [[IO ()]]
-- strToPrint strs = map (map putStrLn) (strs) 



--         uniq :: Eq a => [a] -> [a]
-- uniq [] = []
-- uniq (x:xs) = x : uniq (filter (/=x) xs)
 unique = reverse . nub . reverse

 gameWithComputer initialBoard = do
     putStrLn "Enter 1 to display Board"
     putStrLn "Enter 2 to add a word to the existing Board"
     putStrLn "Enter 3 to input letters for computer"
     line <- getLine
     if null line
         then return()
     else
         if line == "1"
             then do 
                 printBoard initialBoard
                 return ()
                 gameWithComputer initialBoard
         else
             if line == "2"
                 then do
                     putStrLn "Enter word to be added to the board"
                     word <- getLine
                     putStrLn "Enter coordinate of the starting index of the word ( (0,0) denotes \nthe top left box and (12,12) denotes the bottom right box"
                     coordinate' <- getLine
                     let coordinate = read coordinate' :: (Int,Int)
                     putStrLn "Enter orientation (H for horizontal and V for vertical)"
                     orientation <- getLine
                     -- let orientation = read orientation' :: Char
                     
                     if orientation == "H"
                         then do
                                 if (snd(coordinate) + length(word)) > 12 
                                     then do
                                         putStrLn "Word cannot be fitted in the board"
                                         gameWithComputer initialBoard
                                         return ()
                                     else do
                                         putStrLn "Word can be fitted in the board"
                                         return ()
                                    

                             --Checks if the word is in dictionary
                                 if search word == True
                                     then do
                                         putStrLn "Right word"
                                         return ()
                                     else do
                                         putStrLn "word not found in dictionary"
                                         printBoard initialBoard
                                         gameWithComputer initialBoard
                                         return ()
 
                             --Checks if the new word added is overwriting the board
                                 if check initialBoard (listOfPoints (coordinate,(fst(coordinate),snd(coordinate) + length (word) -1))) word == True
                                     then do 
                                         putStrLn "right"
                                         return ()
                                     else do
                                         putStrLn "Wrong addition of word"
                                         printBoard initialBoard
                                         gameWithComputer initialBoard
                                         return ()
                                 putStrLn "Modified Board is .............."
                                 printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                 gameWithComputer $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate))word initialBoard
                                 return ()
                     else do
                             --Checks if the word is in dictionary
                             if search word == True
                                 then do
                                     putStrLn "Right word"
                                     return ()
                                 else do
                                     putStrLn "word not found in dictionary"
                                     printBoard initialBoard
                                     gameWithComputer initialBoard
 
                             --Checks if the new word added is overwriting the board
                             if check initialBoard (listOfPoints (coordinate,(fst(coordinate)  + length (word) -1,snd(coordinate)))) word == True
                                 then do 
                                     putStrLn "right"
                                     return ()
                                 else do
                                     putStrLn "Wrong addition of word"
                                     printBoard initialBoard
                                     gameWithComputer initialBoard
                                     return ()
                             putStrLn "Modified Board is .............."
                             printBoard $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                             gameWithComputer $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                             return () 
                     return ()
                 else
                     do 
                        putStrLn "Enter 7 letters for me to form a word"
                        letters <- getLine
                        if (length letters) /= 7
                             then do 
                                 putStrLn "Game Over as you didn't enter 7 letters"
                                 gameWithComputer initialBoard
                        else 
                            do
                             return ()
                        let board = initialBoard

                        -- filters the tuples , returns a list of tuples [((int,int),(int.int))]
                        let rightTuplesH = (findPossTuples board listOfTuplesH 13)
                        let rightTuplesV = (findPossTuples board listOfTuplesV 13)                        
                        let rightTuples = rightTuplesH ++ rightTuplesV

                        -- finding list of points for all


                        -- forms the words by first making a (Point,char array of the tuple and then passing it to findAllPermutations)
                        -- naman . error - variable not in scope
                        let words = unique $ concat $ [findAllWords (listOfP initialBoard tuple) letters | tuple <- rightTuples ]
                        print [findAllWords (listOfP initialBoard tuple) letters | tuple <- rightTuples ]
                        print words
                        putStrLn "Found Words"
                        --divyanshu . error - variable not in scope
                        -- let words = map (findAllPermutations)  (map (listOfP) board rightTuples) letters
                        -- words sahi arahe hai putSTrLn kaam nhi kar raha hai . dekhle
                        -- strToPrint function is for printing [[[Char]]]
                         --strToPrint words
                        --  $ unique $ concat words
                        --print words
                        
                        -- niche wala hardcoded example chal raha hai
                        --let words = ["cat","apple","dog","ghci","haskell"]
                        --check if the words are in the dictionary 
                        let correct_words = [word | word <- words , (search word) == True]
                        print correct_words
                        putStrLn "Correct Words"

                        
                        -- putStrLn (correct_words !! 0)
                     --    putStrLn rightTuples
                     --    putStrLn words
                        
                        -- sort the words according to their scores 
                        let sorted = sortWords correct_words
                        putStrLn "Sorted"
                        putStrLn (last sorted)    
                        
                     --    score calculation 
                     --    sorting of words according to score
                     --    finding orientation of the word
                         
                        return () 
                        putStrLn "maa chuda"
                        let coordinate = (2,2)
                        let word = "apple"
                        let orientation = "H"
                        if orientation == "H"
                          then do
                             putStrLn "Modified Board is .............."
                             printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                             gameWithComputer $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                             return ()
                          else do
                             putStrLn "Modified Board is .............."
                             printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                             gameWithComputer $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                             return () 
                        
           
 --generates 7 random letters                               
 input:: [[Char]]
 input = do
     let num1 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num2 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num3 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num4 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num5 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num6 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     let num7 = unsafePerformIO (getStdRandom (randomR (0, 25)))
     return [randomChar num1 , randomChar num2 , randomChar num3,randomChar num4,randomChar num5,randomChar num6,randomChar num7]
 
 -- list of horizontal tuples , eg ((5,3),(8,3))
 listOfTuplesH = [((a,b),(a,c)) | a <- [0..12] , b <- [0..12] , c <- [0..12], c - b > 1]
 
 -- list of vertical tuples , eg (()) 
 listOfTuplesV = [((b,a),(c,a)) | a <- [0..12] , b <- [0..12] , c <- [0..12], c - b > 1]
 
 
