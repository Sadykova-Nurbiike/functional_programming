module ex3

import StdEnv
 
// 1. Compute the product of the elements of a list
product :: [Int] -> Int
/*product [] = 1
product [x:xs] = x* product xs*/
product x = prod x

//Start = product [1..5] // 120

// 2. delete the elements equal to 5
not_five::[Int] ->[Int]
not_five [] = []
not_five [h:t] 
|h==5 = not_five t
= [h: not_five t]

//Start = not_five [5,4,5,4,3]  // [4,4,3]

// 3. Delete an element n from a list
del :: Int [Int] -> [Int]
del n [] = []
del n [x:xs] 
|x==n = del n xs
=[x:del n xs]

//Start = del 5 [1, 5, 6, 7, 5, 8, 5] // [1, 6, 7, 8]


// 4. write a funtion with the patterns depending on the parameter:
// if the param is [] then is equal to 20, if is a two element list starting with 4 then is 30
// if is a two element list ending with 5 then is 40, in all other cases is 50, 
// the order of the patterns is important
gp :: [Int] -> Int
gp [] = 20
gp [4,_] = 30
gp [_,5] = 40
gp list = 50
//Start = gp [1,8] // 50


// 5. write a funtion which returns true if a is divisible by b
div_by :: Int Int -> Bool
div_by a b
|(a rem b == 0)= True
= False

//Start = div_by 16 4      // True


// 6. write a funtion which returns true if a is divisible by b or vice versa
div_any :: Int Int -> Bool
div_any a b
|(a rem b == 0)||(b rem a == 0)= True
=False

// Start = div_any 4 16     // True


// 7. sumsq n returns 1*1 + 2*2 + ... + n*n - with a pattern for 0
sumsq::Int -> Int
sumsq x 
|x== 0 = 0
= foldr (+)0 (map (\x = x*x) [1..x])

//Start = sumsq 3 // 14

sumq::Int -> Int
sumq x
|x == 0 = 0
= sum (map (\x = x*x) [1..x])
//Start = sumq 3

// version 2. - without pattern for 0
//sums :: Int -> Int
sums:: Int -> Int
sums n
|n==1 = 1
= n*n + sums (n-1)
//Start = sums 3
sup:: Int -> Int
sup x = foldr (+)0 (map (\x=x*x) [1..x])
//Start = sup 3
  

// 8. check if a number is palindrom e.g.12321
p :: Int -> [Int]
p x 
|x == 0 = []
= p(x/10) ++ [x rem 10]

r:: [Int] -> Int
r [] = 0
r x = (10 * r (init x)) + last x
//Start = r [1..5]

q:: Int -> [Int]
q x
|x == 0 = []
= [x rem 10] ++ q(x/10)
//digits :: Int [Int] -> [Int]

/*
pali :: Int -> Bool
pali x
|p x == q x = True
=False
//Start = pali 12321 // True
*/

pali2 :: Int -> Bool
pali2 x
| length (p x) < 2 = True
| hd (p x) <> last (p x) = False
| hd (p x) == last (p x) = pali2 (r ( init ( tl (p x) ) ))

//Start = pali2 1234321222


// 9. Computes the nth Fibonacci number - try more versions
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
//Start = fib 3
fib1 :: Int -> (Int, Int)
fib1 0 = (1,1)
fib1 1 = (1,1)
fib1 n = (b,a+b)
where
	(a,b) = fib1 (n-1)

//Start = fib1 3
fib2 :: Int Int Int -> Int
fib2 a b 0 = a
fib2 a b c = fib2 b (a+b) (c-1)

// Start = fib2 1 1 10

fib3 :: Int -> Int
fib3 n = fibAux n 1 1

fibAux 0 a b         = a
fibAux i a b | i > 0 = fibAux (i-1) b (a+b)

// Start = fib3 8


// 10. exists x xs checks whether x exists as an element in the list xs (or is ||, and is &&)
exists :: Int [Int] -> Bool
exists x [] = False
exists x [h:t] = (x==h)||exists x t
//Start = exists 3 [1..5]

// 11. write the function duplicates which checks if there are duplicates in the list xs
duplicates :: [Int] -> Bool
duplicates [] = False
duplicates [x:xs] = exists x xs || duplicates xs
//Start = duplicates [1, 2, 3, 41, 2, 3, 2, 1, 3] // True


// 12. remove x xs removes x from the list xs
remove :: Int [Int] -> [Int]
remove x [] = []
remove x [h:t]
|x == h  = remove x t
=[h] ++ remove x t
//= [h:remove x t]

//Start = remove 3 [1,3,6,3,7]

// 13. removeDuplicates l returns the list l with all duplicate elements removed
removeDuplicates :: [Int] -> [Int]
removeDuplicates [] = []
removeDuplicates [h:t] = [h: removeDuplicates (remove h t)]

//Start = removeDuplicates [1, 2, 1, 2, 3, 1, 2, 4, 2, 3]


// 14. filter the elements that are satisfying a condition
filter` :: (Int -> Bool) [Int] -> [Int]
filter` p [] = []
filter` p [x:xs] 
|p x = [x: filter` p xs]
= filter` p xs

Start = filter` ((<>) 5) [1, 5, 6, 7, 5, 8, 5]  // [1, 6, 7, 8] 