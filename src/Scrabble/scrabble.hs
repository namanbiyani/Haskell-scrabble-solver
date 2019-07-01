module Scrabble (
   module Scrabble.Board,
) where

import Scrabble.Board
import System.Random

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
                then do putStrLn "Enter 1 to print Current Board"
                        putStrLn "Enter 2 to add a word to Current Board"
                        putStrLn "Enter 3 to start Game"
                else
                     if line == "2"
                        then do putStrLn "Enter 1 to print Current Board"
                                putStrLn "Enter 2 to add a word to Current Board"
                                putStrLn "Enter 7 letters for me"
                     else
                        do
                          putStrLn "wrong choice"
                          return()

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
