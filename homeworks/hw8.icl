module hw8
import StdEnv

f1 :: Int -> [[Int]]
f1 n = [[1..x] \\ x <- [1..n]]

//Start = f1 10


f2_aux :: [Int] -> Int
f2_aux [] = 1
f2_aux x = prod x

f2 :: [[Int]] -> [(Int, Int)]
f2 list = [(a,b) \\ a <- [1..(length list)] & b <- map (f2_aux) list]

//Start = f2  [[1,2,3], [4,5], [6,1,8], []] 


f3 :: [Int]
f3 = take 100 [x \\ x <- [25,50..] | x rem 100 <> 0]

//Start = f3


