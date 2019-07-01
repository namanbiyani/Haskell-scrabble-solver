{-# LANGUAGE BlockArguments #-}


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
           then print $Main.words word  0 719 $lines base_a
         else if y == 'b'
           then print $Main.words word 0 535 $lines base_b
         else if y == 'c'
           then print $Main.words word 0 1014 $lines base_c
         else if y == 'd'
           then print $Main.words word 0 560 $lines base_d
         else if y == 'e'
           then print $Main.words word 0 465 $lines base_e
         else if y == 'f'
           then print $Main.words word 0 431 $lines base_f
         else if y == 'g'
           then print $Main.words word 0 288 $lines base_g
         else if y == 'h'
           then print $Main.words word 0 333 $lines base_h
         else if y == 'i'
           then print $Main.words word 0 385 $lines base_i 
         else if y == 'j'
           then print $Main.words word 0 126 $lines base_j
         else if y == 'k'
           then print $Main.words word 0 97 $lines base_k
         else if y == 'l'
           then print $Main.words word 0 371 $lines base_l
         else if y == 'm'
           then print $Main.words word 0 554 $lines base_m
         else if y == 'n'
           then print $Main.words word 0 230 $lines base_n
         else if y == 'o'
           then print $Main.words word 0 225 $lines base_o
         else if y == 'p'
           then print $Main.words word 0 791 $lines base_p
         else if y == 'q'
           then print $Main.words word 0 45 $lines base_q
         else if y == 'r'
           then print $Main.words word 0 580 $lines base_r
         else if y == 's'
           then print $Main.words word 0 1004 $lines base_s
         else if y == 't'
           then print $Main.words word 0 548 $lines base_t
         else if y == 'u'
           then print $Main.words word 0 126 $lines base_u
         else if y == 'v'
           then print $Main.words word 0 189 $lines base_v
         else if y == 'w'
           then print $Main.words word 0 288 $lines base_w
         else if y == 'x'
           then print $Main.words word 0 11 $lines base_x
         else if y == 'y'
           then print $Main.words word 0 42 $lines base_y
         else print $Main.words word 0 21 $lines base_z
    

words value low high xs 
   | high < low       = False
   | xs!!mid > value  = Main.words value low (mid-1) xs
   | xs!!mid < value  = Main.words value (mid+1) high xs
   | otherwise        = True
   where
   mid = low + ((high - low) `div` 2)
