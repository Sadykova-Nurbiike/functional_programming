module examples1
import StdEnv 

//Start = drop ([1..5]!!2) [1..5]

//reverse sublists of list
revsub :: [[Int]] -> [[Int]]
revsub [] = [] 
revsub [x:xs] = [reverse x : revsub xs]

//Start = revsub [[1, 2],[3, 4]] 

//Start = [0,5..500]

//Delete the first and the last element of a list
delFL:: [Int] -> [Int]
delFL [] = []
delFL [x] = []
delFL [x:xs] = init xs

delFL1 :: [Int] -> [Int]
delFL1 x
| length x > 1 = init (tl x)
=[]

//Start = delFL1 [0,2..10]

//I
sumI :: Int -> Int
sumI n
|n < 0 = abort "n should be positive"
| n==0 = 0
= (2*n)*(2*n+1) + sumI (n-1)

//Start = sumI 3 

//II
f :: Int -> Int
f n
| n < 1 = abort " n should be positive"
= g 1 n

g :: Int Int -> Int
g k n
| k == n+1 = 0
=2*k*(2*k+1) + g (k+1) n

//Start = f 3


cutMid :: [Int] -> [[Int]]
cutMid x = [take n x ,drop n x]
where n = length x / 2
           
//Start = cutMid [1..11] 


LastSub :: [[Int]] -> [Int]
LastSub [] = []
LastSub [x:xs] = [last x] ++ LastSub xs

//Start = LastSub [[1, 2], [1, 2], [1, 2]]

//I
Middle :: [Int] -> Int
Middle x = hd (drop n x)
where n = length x / 2

//II
middle x = x!!n
where n = length x / 2

//Start = middle [1..4]

//I
ins0 :: [[Int]] -> [[Int]]
ins0 [] = []
ins0 [x:xs] = [[0]++x : ins0 xs]

//Start = ins02 [[1,2],[1,2,3]]

//II
ins01 :: [[Int]] -> [[Int]]
ins01 [] = []
ins01 [x:xs] = [flatten [[0],x] : ins01 xs]

//III
ins02 :: [[Int]] -> [[Int]]
ins02 [] = []
ins02 [x:xs] = [[0:x] : ins0 xs] 

//
delLsub :: [[Int]] -> [[Int]]
delLsub [] = []
delLsub [x:xs] = [init x : delLsub xs] 

Start = delLsub [[1..10],[1..3]]




 
 
         


