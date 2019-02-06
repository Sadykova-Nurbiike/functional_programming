module hw5

import StdEnv

secEl :: Int [Int]  -> [Int]
secEl n [] = [n]
secEl n [x:xs]  = [x:[n:xs]]

f1 :: [[Int]] Int -> [[Int]] 
f1 list n = map ((secEl) n ) list

//Start = f1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 10  // \(~_~)/

f2 :: [Int] -> Bool
f2 [] = False
f2 [x] = False
f2 [x:xs] 
| x == hd xs = True
= f2 xs

//Start = f2 [1,2,2,3,5,5]


f3help :: [Int] -> [Int]
f3help list
| length list < 2 = []
= [list!!1]

f3 :: [[Int]] -> [Int]
f3 list  = flatten (map (f3help) list)

//Start = f3 [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]]

//Start = (f1 [[1,2], [3,4,5], [6,5,9,7], [], [8]] 66, f2 [1,2,3,4,2,2], f3 [[1,2,3], [3,4,5,6], [], [5,7,8,11], [1], [8,9]])



