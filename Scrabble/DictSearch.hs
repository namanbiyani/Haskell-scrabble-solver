module DictSearch
( binarysearch,
  search,
) where

import System.IO.Unsafe
  
binarysearch :: String -> Int -> Int -> [String] -> Bool 
binarysearch value low high xs 
   | high < low       = False
   | xs!!mid > value  = binarysearch value low (mid-1) xs
   | xs!!mid < value  = binarysearch value (mid+1) high xs
   | otherwise        = True
   where
   mid = low + ((high - low) `div` 2)

search :: String -> Bool
search word =   
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

  where
    base_a = unsafePerformIO .readFile $ "../dict/a.txt"       
    base_b = unsafePerformIO .readFile $ "../dict/b.txt"
    base_c = unsafePerformIO .readFile $ "../dict/c.txt"
    base_d = unsafePerformIO .readFile $ "../dict/d.txt"
    base_e = unsafePerformIO .readFile $ "../dict/e.txt"
    base_f = unsafePerformIO .readFile $ "../dict/f.txt"
    base_g = unsafePerformIO .readFile $ "../dict/g.txt"
    base_h = unsafePerformIO .readFile $ "../dict/h.txt"
    base_i = unsafePerformIO .readFile $ "../dict/i.txt"
    base_j = unsafePerformIO .readFile $ "../dict/j.txt"
    base_k = unsafePerformIO .readFile $ "../dict/k.txt"
    base_l = unsafePerformIO .readFile $ "../dict/l.txt"
    base_m = unsafePerformIO .readFile $ "../dict/m.txt"
    base_n = unsafePerformIO .readFile $ "../dict/n.txt"
    base_o = unsafePerformIO .readFile $ "../dict/o.txt"
    base_p = unsafePerformIO .readFile $ "../dict/p.txt"
    base_q = unsafePerformIO .readFile $ "../dict/q.txt"
    base_r = unsafePerformIO .readFile $ "../dict/r.txt"
    base_s = unsafePerformIO .readFile $ "../dict/s.txt"
    base_t = unsafePerformIO .readFile $ "../dict/t.txt"
    base_u = unsafePerformIO .readFile $ "../dict/u.txt"
    base_v = unsafePerformIO .readFile $ "../dict/v.txt"
    base_w = unsafePerformIO .readFile $ "../dict/w.txt"
    base_x = unsafePerformIO .readFile $ "../dict/x.txt"
    base_y = unsafePerformIO .readFile $ "../dict/y.txt"
    base_z = unsafePerformIO .readFile $ "../dict/z.txt"
    y = word !! 0