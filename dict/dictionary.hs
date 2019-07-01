-- {-# LANGUAGE BlockArguments #-}


main = do
         putStrLn "word which is to be checked"
         word <- getLine 
         base_a <- readFile "a.txt"       
         base_b <- readFile "b.txt"
         base_c <- readFile "c.txt"
         base_d <- readFile "d.txt"
         base_e <- readFile "e.txt"
         base_f <- readFile "f.txt"
         base_g <- readFile "g.txt"
         base_h <- readFile "h.txt"
         base_i <- readFile "i.txt"
         base_j <- readFile "j.txt"
         base_k <- readFile "k.txt"
         base_l <- readFile "l.txt"
         base_m <- readFile "m.txt"
         base_n <- readFile "n.txt"
         base_o <- readFile "o.txt"
         base_p <- readFile "p.txt"
         base_q <- readFile "q.txt"
         base_r <- readFile "r.txt"
         base_s <- readFile "s.txt"
         base_t <- readFile "t.txt"
         base_u <- readFile "u.txt"
         base_v <- readFile "v.txt"
         base_w <- readFile "w.txt"
         base_x <- readFile "x.txt"
         base_y <- readFile "y.txt"
         base_z <- readFile "z.txt"
         let y = word !! 0 
         
         if y == 'a'
           then print $Main.words word $lines base_a
         else if y == 'b'
           then print $Main.words word $lines base_b
         else if y == 'c'
           then print $Main.words word $lines base_c
         else if y == 'd'
           then print $Main.words word $lines base_d
         else if y == 'e'
           then print $Main.words word $lines base_e
         else if y == 'f'
           then print $Main.words word $lines base_f
         else if y == 'g'
           then print $Main.words word $lines base_g
         else if y == 'h'
           then print $Main.words word $lines base_h
         else if y == 'i'
           then print $Main.words word $lines base_i
         else if y == 'j'
           then print $Main.words word $lines base_j
         else if y == 'k'
           then print $Main.words word $lines base_k
         else if y == 'l'
           then print $Main.words word $lines base_l
         else if y == 'm'
           then print $Main.words word $lines base_m
         else if y == 'n'
           then print $Main.words word $lines base_n
         else if y == 'o'
           then print $Main.words word $lines base_o
         else if y == 'p'
           then print $Main.words word $lines base_p
         else if y == 'q'
           then print $Main.words word $lines base_q
         else if y == 'r'
           then print $Main.words word $lines base_r
         else if y == 's'
           then print $Main.words word $lines base_s
         else if y == 't'
           then print $Main.words word $lines base_t
         else if y == 'u'
           then print $Main.words word $lines base_u
         else if y == 'v'
           then print $Main.words word $lines base_v
         else if y == 'w'
           then print $Main.words word $lines base_w
         else if y == 'x'
           then print $Main.words word $lines base_x
         else if y == 'y'
           then print $Main.words word $lines base_y
         else print $Main.words word $lines base_z 
    
words :: String -> [String] -> String
words word (x:xs) 
           | word == x = "True"
           | otherwise = Main.words word xs

words _ _ = "False"