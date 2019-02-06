module ex11
import StdEnv 

// 1. Triple a number.
triple1 :: Int -> Int
triple1 x = x+x+x

//Start = triple1 3


// 2. Check if a number is odd.
isoddnr :: Int -> Bool
isoddnr x = (x rem 2 == 1)
//Start = isoddnr 6


// 3. Check if a number is the sum of two other given numbers.
issum :: Int Int Int -> Bool
issum x y z = (x == y+z)

//Start = issum 10 6 3


// 4. Add 100 to a number.
add100 :: Int -> Int
add100 x = x + 100
//Start = add100 5


// 5. Check if a number is multiple of 10.
ismult10 :: Int -> Bool
ismult10 x = (x rem 10 == 0)
//Start = ismult10 20


// 6. Add the numbers from 1..N in a recursive function.
addn :: Int -> Int
addn x 
|x == 0 = 0
|x > 0 = x + addn (x-1)
//Start = addn 5


// 7. Compute the N choose K value.
fac :: Int -> Int
fac x 
|x == 1 = 1
|x > 0 = x * fac (x-1)
choose_k :: Int Int -> Int
choose_k x y = fac x / ((fac y)*(fac (x-y)))

//Start = choose_k 10 4

f n k
| k == 0 = 1
= (n-k+1)* f n (k-1)

g n k 
| k == 0 = 1
= k * g n (k-1)

n_choose_k :: Int Int -> Int
n_choose_k n k = f n k / g n k

//Start = n_choose_k 5 2

nchoosek :: Int Int -> Int
nchoosek n k
|k==0 = 1
= n * nchoosek (n-1) (k-1) / k
//Start = nchoosek 7 3

nchoosek1:: Int Int -> Int 
nchoosek1 n k
|k==0 = 1 
= (n-k+1) * nchoosek1 n (k-1) / k
//Start = nchoosek 10 4

// 8. Compute the cube of a number
cube :: Int -> Int
cube x = x* x * x

//Start = cube 4


// 9*. Define addition as recursive function.
recadd :: Int -> Int
recadd x 
|x== 0 = 0
= x + recadd (x - 1)

Start = recadd 5