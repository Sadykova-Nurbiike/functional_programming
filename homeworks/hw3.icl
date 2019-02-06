module hw3
import StdEnv

f1 :: Int -> [Int]
f1 x
|  (x rem 10) == 0  = [x]
= f1 (x/10) ++ [x rem 10]

f2 :: [Int]->[Int]
f2 []=[]
f2 [x:xs]
| x==0 = [0 : f2 xs]
= [x-1 : f2 xs]

f3 :: [Int] -> Int
f3 [] = 0
f3 list = (f3 (map ((*)10) (init list)) + (last list)

f :: Int -> Int
f x = f3 (f2 (f1 x))

Start = f 54321
