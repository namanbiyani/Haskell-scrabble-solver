-- --TODO : the main function and all the  stuff needs to deleted . there would be a 
-- --search function(subset of main function) which inputs a word and outputs a boolean
-- --Follow the module Function
-- module DictSearch
-- (
--   search,--this function replaces main function
--    binarysearch,--this should replace the words function
-- )
-- where

-- main = do
--    "word which is to be checked"
--   word <- getLine 
--     base_a <- readFile "../dict/../dict/a.txt"       
--     base_b <- readFile "../dict/../dict/b.txt"
--     base_c <- readFile "../dict/../dict/c.txt"
--     base_d <- readFile "../dict/../dict/d.txt"
--     base_e <- readFile "../dict/../dict/e.txt"
--     base_f <- readFile "../dict/../dict/f.txt"
--     base_g <- readFile "../dict/../dict/g.txt"
--     base_h <- readFile "../dict/../dict/h.txt"
--     base_i <- readFile "../dict/../dict/i.txt"
--     base_j <- readFile "../dict/../dict/j.txt"
--     base_k <- readFile "../dict/../dict/k.txt"
--     base_l <- readFile "../dict/../dict/l.txt"
--     base_m <- readFile "../dict/../dict/m.txt"
--     base_n <- readFile "../dict/../dict/n.txt"
--     base_o <- readFile "../dict/../dict/o.txt"
--     base_p <- readFile "../dict/../dict/p.txt"
--     base_q <- readFile "../dict/../dict/q.txt"
--     base_r <- readFile "../dict/../dict/r.txt"
--     base_s <- readFile "../dict/../dict/s.txt"
--     base_t <- readFile "../dict/../dict/t.txt"
--     base_u <- readFile "../dict/../dict/u.txt"
--     base_v <- readFile "../dict/../dict/v.txt"
--     base_w <- readFile "../dict/../dict/w.txt"
--     base_x <- readFile "../dict/../dict/x.txt"
--     base_y <- readFile "../dict/../dict/y.txt"
-- - = -     base_z <- readFile "../dict/../dict/z.txt"
--   let y = word =  !! 0 
  
--   if y == 'a' = 
--     then  binarysearch word  0 719 $lines base_a
--   else if y == 'b' = 
--     then  binarysearch word 0 535 $lines base_b
--   else if y == 'c' = 
--     then  binarysearch word 0 1014 $lines base_c
--   else if y == 'd' = 
--     then  binarysearch word 0 560 $lines base_d
--   else if y == 'e' = 
--     then  binarysearch word 0 465 $lines base_e
--   else if y == 'f' = 
--     then  binarysearch word 0 431 $lines base_f
--   else if y == 'g' = 
--     then  binarysearch word 0 288 $lines base_g
--   else if y == 'h' = 
--     then  binarysearch word 0 333 $lines base_h
--   else if y == 'i' = 
--     then  binarysearch word 0 385 $lines base_i 
--   else if y == 'j' = 
--     then  binarysearch word 0 126 $lines base_j
--   else if y == 'k' = 
--     then  binarysearch word 0 97 $lines base_k
--   else if y == 'l' = 
--     then  binarysearch word 0 371 $lines base_l
--   else if y == 'm' = 
--     then  binarysearch word 0 554 $lines base_m
--   else if y == 'n' = 
--     then  binarysearch word 0 230 $lines base_n
--   else if y == 'o' = 
--     then  binarysearch word 0 225 $lines base_o
--   else if y == 'p' = 
--     then  binarysearch word 0 791 $lines base_p
--   else if y == 'q' = 
--     then  binarysearch word 0 45 $lines base_q
--   else if y == 'r' = 
--     then  binarysearch word 0 580 $lines base_r
--   else if y == 's' = 
--     then  binarysearch word 0 1004 $lines base_s
--   else if y == 't' = 
--     then  binarysearch word 0 548 $lines base_t
--   else if y == 'u' = 
--     then  binarysearch word 0 126 $lines base_u
--   else if y == 'v' = 
--     then  binarysearch word 0 189 $lines base_v
--   else if y == 'w' = 
--     then  binarysearch word 0 288 $lines base_w
--   else if y == 'x' = 
--     then  binarysearch word 0 11 $lines base_x
--   else if y == 'y' = 
--    =   then  binarysearch word 0 42 $lines base_y
--   else  binarysearch word 0 21 $lines base_z


-- words value low high xs 
--    | high < low       = False
--    | xs!!mid > value  = Main.words value low (mid-1) xs
--    | xs!!mid < value  = Main.words value (mid+1) high xs
--    | otherwise        = True
--   where
--    mid = low + ((high - low) `div` 2)


module Search
( binarysearch,
  readfile,
) where

import System.IO
  
binarysearch :: String -> Int -> Int -> [String] -> Bool 
binarysearch value low high xs 
   | high < low       = False
   | xs!!mid > value  = binarysearch value low (mid-1) xs
   | xs!!mid < value  = binarysearch value (mid+1) high xs
   | otherwise        = True
   where
   mid = low + ((high - low)   `div` 2)

readfile :: String -> Bool   
readfile word = do 
  base_a <- readFile "../dict/a.txt"      
  base_b <- readFile "../dict/b.txt"
  base_c <- readFile "../dict/c.txt"
  base_d <- readFile "../dict/d.txt"
  base_e <- readFile "../dict/e.txt"
  base_f <- readFile "../dict/f.txt"
  base_g <- readFile "../dict/g.txt"
  base_h <- readFile "../dict/h.txt"
  base_i <- readFile "../dict/i.txt"
  base_j <- readFile "../dict/j.txt"
  base_k <- readFile "../dict/k.txt"
  base_l <- readFile "../dict/l.txt"
  base_m <- readFile "../dict/m.txt"
  base_n <- readFile "../dict/n.txt"
  base_o <- readFile "../dict/o.txt"
  base_p <- readFile "../dict/p.txt"
  base_q <- readFile "../dict/q.txt"
  base_r <- readFile "../dict/r.txt"
  base_s <- readFile "../dict/s.txt"
  base_t <- readFile "../dict/t.txt"
  base_u <- readFile "../dict/u.txt"
  base_v <- readFile "../dict/v.txt"
  base_w <- readFile "../dict/w.txt"
  base_x <- readFile "../dict/x.txt"
  base_y <- readFile "../dict/y.txt"
  base_z <- readFile "../dict/z.txt"
  let y = word !! 0 

  if y == 'a' 
        then  binarysearch word 0 719 $lines base_a
  else if y == 'b' 
        then  binarysearch word 0 535 $lines base_b
  else if y == 'c' 
        then  binarysearch word 0 1014 $lines base_c
  else if y == 'd' 
        then  binarysearch word 0 560 $lines base_d
  else if y == 'e' 
        then  binarysearch word 0 465 $lines base_e
  else if y == 'f'  
        then  binarysearch word 0 431 $lines base_f
  else if y == 'g'  
        then  binarysearch word 0 288 $lines base_g
      else if y == 'h'  
        then  binarysearch word 0 333 $lines base_h
      else if y == 'i'  
        then  binarysearch word 0 385 $lines base_i 
      else if y == 'j'  
        then  binarysearch word 0 126 $lines base_j
      else if y == 'k'  
        then  binarysearch word 0 97 $lines base_k
      else if y == 'l'  
        then  binarysearch word 0 371 $lines base_l
      else if y == 'm'  
        then  binarysearch word 0 554 $lines base_m
      else if y == 'n'  
        then  binarysearch word 0 230 $lines base_n
      else if y == 'o'  
        then  binarysearch word 0 225 $lines base_o
      else if y == 'p'  
        then  binarysearch word 0 791 $lines base_p
      else if y == 'q'  
        then  binarysearch word 0 45 $lines base_q
      else if y == 'r'  
        then  binarysearch word 0 580 $lines base_r
      else if y == 's'  
        then  binarysearch word 0 1004 $lines base_s
      else if y == 't'  
        then  binarysearch word 0 548 $lines base_t
      else if y == 'u'  
        then  binarysearch word 0 126 $lines base_u
      else if y == 'v'  
        then  binarysearch word 0 189 $lines base_v
      else if y == 'w'  
        then  binarysearch word 0 288 $lines base_w
      else if y == 'x'  
        then  binarysearch word 0 11 $lines base_x
      else if y == 'y'  
         then  binarysearch word 0 42 $lines base_y
      else  binarysearch word 0 21 $lines base_z
    

  -- return ()

-- search word 
--   | y == 'a' =  binarysearch word  0 719 $lines base_a
--   | y == 'b' =  binarysearch word 0 535 $lines base_b
--   | y == 'c' =  binarysearch word 0 1014 $lines base_c
--   | y == 'd' =  binarysearch word 0 560 $lines base_d
--   | y == 'e' =  binarysearch word 0 465 $lines base_e
--   | y == 'f' =  binarysearch word 0 431 $lines base_f
--   | y == 'g' =  binarysearch word 0 288 $lines base_g
--   | y == 'h' =  binarysearch word 0 333 $lines base_h
--   | y == 'i' =  binarysearch word 0 385 $lines base_i
--   | y == 'j' =  binarysearch word 0 126 $lines base_j
--   | y == 'k' =  binarysearch word 0 97 $lines base_k
--   | y == 'l' =  binarysearch word 0 371 $lines base_l
--   | y == 'm' =  binarysearch word 0 554 $lines base_m
--   | y == 'n' =  binarysearch word 0 230 $lines base_n
--   | y == 'o' =  binarysearch word 0 225 $lines base_o
--   | y == 'p' =  binarysearch word 0 791 $lines base_p
--   | y == 'q' =  binarysearch word 0 45 $lines base_q
--   | y == 'r' =  binarysearch word 0 580 $lines base_r
--   | y == 's' =  binarysearch word 0 1004 $lines base_s
--   | y == 't' =  binarysearch word 0 548 $lines base_t
--   | y == 'u' =  binarysearch word 0 126 $lines base_u
--   | y == 'v' =  binarysearch word 0 189 $lines base_v
--   | y == 'w' =  binarysearch word 0 288 $lines base_w
--   | y == 'x' =  binarysearch word 0 11 $lines base_x
--   | y == 'y' =  binarysearch word 0 42 $lines base_y
--   | y == 'z' =  binarysearch word 0 21 $lines base_z
--   where y = word !! 0  
  
        
    
