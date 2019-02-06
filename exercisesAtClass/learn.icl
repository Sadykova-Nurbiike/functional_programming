module learn

import StdEnv


//1
//Start = 42


//2
func1 :: Int Int -> Int
func1 x n = x^n

func2 :: Int -> Int
func2 x = func1 x 2

//Start = func2 12


//3
isum :: Int -> Int
isum n 
| n==0 = 0
= n rem 10 + isum (n/10)

//Start = isum 5 


//4
divBy9 :: Int -> Bool
divBy9 n = ((isum n) rem 3 == 0) 

//Start = divBy9 50


//5
Max :: Int Int -> Int
Max a b
|a>=b = a
=b

//Start = Max -5 -5


//6
Min :: Int Int -> Int
Min a b 
|a>=b = b
=a

//Start = Min 9 5


//7
MaxOfList :: [Int] -> Int
MaxOfList [x] = x
MaxOfList [x:xs]
| x <= hd xs = MaxOfList xs
= MaxOfList ([x] ++ (tl xs))

//Start = MaxOfList [0,9,7]

//8
MinOfList :: [Int] -> Int
MinOfList [x] = x
MinOfList [x:xs]
| x >= hd xs = MinOfList xs
= MinOfList ([x] ++ (tl xs))

//Start = MinOfList [0,9,7]


//Start = [n*n \\ n <- [1..10] | n rem 2 <> 0]

//9
Last :: [Int] -> Int
Last [x] = x
Last [x:xs] = hd (reverse [x:xs])

Last2 :: [Int] -> Int
Last2 [x] = x
Last2 [x:xs] = Last2 xs

//Start = Last2 [1..10]

//10
LastTwo :: [Int] -> [Int]
LastTwo [] = []
LastTwo [x] = []
LastTwo [x,y] = [x,y]
LastTwo list = drop ((length list)-2) list

//Start = LastTwo [5,10..75]


//11
Reverse :: [a] -> [a]
Reverse [] = []
Reverse [x:xs] = Reverse xs ++ [x]

//Start = Reverse [1..10]


//12
Palindrome :: [Char] -> Bool
Palindrome [] = True
Palindrome [x] = True
Palindrome [x:xs]
 | x == last [x:xs] = Palindrome (init xs)
 |otherwise = False

Start = Palindrome ['1','2','3','2','1']








