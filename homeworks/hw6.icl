module hw6
import StdEnv

f1 :: Int [Int] -> [Int]
f1 x [] = []
f1 x [y:ys] = [y:[x:f1 x ys]]

//Start = f1 0 [1..4]

f2 :: [Int] -> [Int] 
f2 [] = []
f2 [x:xs] = [x: f2 (drop 1 xs)]

//Start = f2 [1,0,2,0,3,0,4]

//Start = f2 (f1 0 [1..4])
