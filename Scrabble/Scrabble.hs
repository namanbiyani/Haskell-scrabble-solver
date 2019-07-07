
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
                        game2Player initialBoard
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

game2Player :: [((Int, Int), Char)] -> IO ()
game2Player initialBoard = do
        putStrLn "Enter 1 to display Board"
        putStrLn "Enter 2 to add a word to the existing Board"
        putStrLn "Enter 3 to start game "
        line <- getLine
        if null line
            then return ()
            else if line == "1"
                then do 
                        printBoard initialBoard
                        game2Player initialBoard
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
                                    if search word == True
                                        then do
                                            putStrLn "Right word"
                                            return ()
                                        else do
                                            putStrLn "word not found in dictionary"
                                            printBoard initialBoard
                                            game2Player initialBoard

                                    --Checks if the new word added is overwriting the board
                                    if check initialBoard (listOfPoints (coordinate,(fst(coordinate),snd(coordinate) + length (word) -1))) word == True
                                        then do 
                                            putStrLn "right"
                                            return ()
                                        else do
                                            putStrLn "Wrong addition of word"
                                            printBoard initialBoard
                                            game2Player initialBoard
                                            return ()

                                    putStrLn "Modified Board is .............."
                                    printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                    --return ()
                                    game2Player $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                    return ()
                                else do
                                    --Checks if the word is in dictionary
                                    if search word == True
                                        then do
                                            putStrLn "Right word"
                                            return ()
                                        else do
                                            putStrLn "Word not found in dictionary"
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
                                    --return ()
                                    game2Player $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                    return () 
                             return () 
                        else
                            do putStrLn "Form a word using the table and the following words"
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
                                    if check initialBoard (listOfPoints (coordinate,(fst(coordinate),snd(coordinate) + length (word) -1))) word == True
                                        then do 
                                            putStrLn "right"
                                            return ()
                                        else do
                                            putStrLn "Wrong addition of word"
                                            printBoard initialBoard
                                            game2Player initialBoard
                                            return ()
                                    
                                    putStrLn "Modified Board is .............."
                                    printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                    
                                    -- score calculation
                                    let score = (show ( calcScore word)) ++ "is the score"
                                    putStrLn score

                                    game2Player $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
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
                                    game2Player $ putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                    return () 
                                    
                               return ()

listOfPoints ((a,c),(b,d)) =  [(x,y) | x <- [a..b] , y <- [c..d]] 

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
                       else do
                            return ()
                       
                    --    filter tuples
                    --    Find possible words from tuples
                    --    Sort words according to score
                        
                       return () 
                       -- if orientation == 'H'
                       --   then do
                       --      putStrLn "Modified Board is .............."
                       --      printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                       --      gameWithComputer $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                       --      return ()
                       --   else do
                       --      putStrLn "Modified Board is .............."
                       --      printBoard $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                       --      gameWithComputer $ putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                       --      return () 
                       
          
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

