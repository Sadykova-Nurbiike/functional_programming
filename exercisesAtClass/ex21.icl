module ex21

import StdEnv




evsub :: [Int] -> [Int]
evsub []=[]
evsub [h:t] = (evsub t)++[h]
//Start = evsub [1..10]

// 1. Reverse every sublist of a list
revsub :: [[Int]] ->  [[Int]]
revsub [] = []
revsub [x:xs] = [reverse x : revsub xs]
//Start = revsub [[1,2,3],[5,6],[],[7,8,9,10]]


// 2. Generate a list with every fifth element form 0 to 500.


//Start =


// 3. Delete the first and the last element of a list.
del_firstlast :: [Int] -> [Int]
del_firstlast [] = []
del_firstlast [x:xs] = take ((length xs)-1) xs

//Start = del_firstlast [1..10]
//Start = del_firstlast []
//Start = del_firstlast [1]
dfl::[Int]->[Int]
dfl x 
|length x > 1 = init (tl x)
= []

// 4. Compute for a given positive n the sum of 2i(2i+1), for i from 1 to n. E.g. for n=3 the sum is 68.
f :: Int -> Int
f x
|x < 0 = abort "n should be positive "
|x == 0 = 0
= 2*x*(2*x + 1) + f (x-1)

//Start = f 3
h::Int -> Int
h n
|n < 1 = abort "n should be positive"
= g 1 n
g::Int Int -> Int
g i n
//| g i n
| i == n + 1 = 0
= 2*i*(2*i+1) + g (i + 1) n



// 5. Cut a list in two parts at the middle. E.g. cut [1..10] -> [[1,2,3,4,5],[6,7,8,9,10]]
// and for cut [1..11] the result is [[1,2,3,4,5],[6,7,8,9,10,11]].

cut::[Int]->[[Int]]
cut x = [take(y/2) x , drop (y/2) x]
where y = length x
//Start = cut [1..11]


//Start = cut []
//Start = cut [1]


// 6. Generate a list with every 500th element form -10000 to 10000.


//Start =


// 7. Keep the last elements of the sublists of a list in one list (the sublists are not empty).
// [[1,2,3],[5,6],[1],[7,8,9,10]] -> [3,6,1,10]
lasts :: [[Int]] -> [Int]
lasts [] = []
lasts [x:xs] = [last x: lasts xs]
//= [last x] ++ lasts xs

//Start = lasts [[1,2,3],[5,6],[1],[7,8,9,10]]


// 8. Extract the middle element of a non-empty list. E.g. for [1..5] is 3, for [1..4] is 3.
middle :: [Int] -> Int
middle x = x!!((length x)/2)

//Start = middle [1..5] 
//Start = middle [1..4] 
//Start = middle [1]


// 9. Instert 0 in front of every sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[0,1,2,3],[0,5,6],[0],[0,7,8,9,10]]
ins0 :: [[Int]] -> [[Int]]
ins0 [] = []
ins0 [x:xs] = [[0]++x : ins0 xs]

//Start = ins0 [[1,2,3],[5,6],[],[7,8,9,10]]
insf::[[Int]] -> [[Int]]
insf [] = []
insf [x:xs] = [ flatten [[0],x] : insf xs]

insz::[[Int]] -> [[Int]]
insz [] = []
insz [x:xs] = [[0:x] : insz xs]

// 10. Delete the last element of each sublist of a list.
// E.g. for [[1,2,3],[5,6],[],[7,8,9,10]] the result is [[1,2],[5],[],[7,8,9]]
lastdel :: [[Int]] -> [[Int]]
lastdel [] = []
lastdel [x:xs] = [init x : lastdel xs]

//Start = lastdel [[1,2,3],[5,6],[],[7,8,9,10]]


// 11. Compute the sum 1+ 2*2+ 3*3*3+ 4*4*4*4+ 5*5*5*5*5+ ...+n*n*n*...*n 
// where n is a positive number.
pow::Int -> Int
pow n = pows n n
pows::Int Int -> Int
pows x y
|y==0 = 1
= x * pows x (y - 1)
sumpowers :: Int -> Int
sumpowers n
|n < 1 = abort "n should be positive"
|n == 1 = 1
= pow n + sumpowers (n - 1)

//Start = sumpowers 3
//Start = sump1 5
//Start = sump2 2


pow1::Int -> Int
pow1 n = pows1 n 1
pows1::Int Int -> Int
pows1 n i
|i == n + 1 = 1
= n * pows1 n (i + 1)
sump1::Int -> Int
sump1 0 = 0
sump1 n = (pow1 n) + sump1 (n - 1)

sump2::Int -> Int
sump2 x
|x == 0 = 0
= x^x + sump2 (x-1)