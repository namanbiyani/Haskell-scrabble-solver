
module Scrabble (
   module Points,
   module Possible_permutations,
   module DictSearch,
   module Change_Board, 
   module Board_new,
   module Bonus,
--    module Filter,
   randomChar,
   input,
   game2Player,
   gameWithComputer,
   listOfTuplesH,
   listOfTuplesV,
) where

import Board_new
import Points
import Possible_permutations
import DictSearch
-- import Filter
import Change_Board
import System.Random
import Data.List
-- import Data.Map
import System.IO
import System.IO.Unsafe
import Data.Char

--putWordAcrs function , (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]
--putWordDown :: (Integer,Integer,Integer) -> [Char] -> [((Integer,Integer),Char)] -> [((Integer,Integer),Char)]

randomChar :: Int->Char
randomChar x =  ['a'..'z'] !! (mod x 26) 

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
                             orientation' <- getLine
                             let orientation = read orientation' :: Char
                             -- check if word addition is possible there
                             -- check if word is correct
                             if orientation == 'H'
                                then do
                                    let newboard = putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                    return ()
                                else do
                                    let newboard = putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                    return () 
                             putStrLn "Modified Board is .............."
                             printBoard newboard
                             game2Player newboard
                        else
                            do putStrLn "Form a word using the table and the following words"
                               printBoard initialBoard
                               let input' = input
                               putStrLn input'
                               putStrLn "Enter your word : "
                               word <- getLine
                               putStrLn "Enter coordinate of the starting index of the word ( (0,0) denotes \nthe top left box and (12,12) denotes the bottom right box"
                               coordinate' <- getLine
                               let coordinate = read coordinate' :: (Int,Int)
                               putStrLn "Enter orientation (H for horizontal and V for vertical)"
                               orientation' <- getLine
                               let orientation = read orientation' :: Char
                               -- check if word addition is possible 
                               -- check if word is valid from DictSearch
                               let score = calcScore word
                               let msg = "Score for the word " ++ show(score) ++ " ! "
                               putStrLn msg
                               if orientation == 'H'
                                then do
                                    let newboard = putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                                    return ()
                                else do
                                    let newboard = putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                                    return () 
                               putStrLn "Modified Board is .............."
                               printBoard newboard

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
                    orientation' <- getLine
                    let orientation = read orientation' :: Char
                    --check if word addition is possible
                    -- check if word is correct    
                    if orientation == 'H'
                        then do
                            let newboard = putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                            return ()
                    else do
                            let newboard = putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                            return () 
                    putStrLn "Modified Board is .............."
                    printBoard newboard
                    gameWithComputer newboard
                else
                    do 
                       putStrLn "Enter 7 letters for me to form a word"
                       letters <- getLine
                       if (length letters) /= 7
                            then do 
                                putStrLn "Game Over as you didn't enter 7 letters"
                       else 
                            return ()
                    --    form possible words  
                    --    score calculation 
                    --    sorting of words according to score
                        
                       return () 
                       if orientation == 'H'
                         then do
                            let newboard = putWordAcrs (fst(coordinate),snd(coordinate),snd(coordinate)+length(word)-1) word initialBoard
                            putStrLn "Modified Board is .............."
                            printBoard newboard
                            gameWithComputer newboard
                            return ()
                         else do
                            let newboard = putWordDown (fst(coordinate),fst(coordinate)+length(word)-1,snd(coordinate)) word initialBoard
                            putStrLn "Modified Board is .............."
                            printBoard newboard
                            gameWithComputer newboard
                            return () 
                       
            
                                 


                               

input:: IO ()
input = do
    num1 <- randomIO :: IO Int
    num2 <- randomIO :: IO Int
    num3 <- randomIO :: IO Int
    num4 <- randomIO :: IO Int
    num5 <- randomIO :: IO Int
    num6 <- randomIO :: IO Int
    num7 <- randomIO :: IO Int
    print $ [randomChar num1 , randomChar num2 , randomChar num3,randomChar num4,randomChar num5,randomChar num6,randomChar num7]

-- list of horizontal tuples , eg ((5,3),(8,3))
listOfTuplesH = [((a,b),(a,c)) | a <- [0..12] , b <- [0..12] , c <- [0..12], c - b > 1]
-- list of vertical tuples , eg (()) 
listOfTuplesV = [((b,a),(c,a)) | a <- [0..12] , b <- [0..12] , c <- [0..12], c - b > 1]

