{-# LANGUAGE BlockArguments #-}


main = do
         putStrLn "word which is to be checked"
         word <- getLine 
         base <- readFile "g.txt"
         print $Main.words word $lines base
    
words :: String -> [String] -> String
words word (x:xs) 
           | word == x = "True"
           | otherwise = Main.words word xs

words _ _ = "False"
