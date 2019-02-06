module hw9
import StdEnv

// 1. Given a list of triple tuples compute for each tuple (x,y,z) the value (2x,y/2,2z+1)
f1 :: [(Int,Int,Int)] -> [(Int,Int,Int)]
f1 list = [(2*x,y/2,2*z+1) \\ (x,y,z) <- list]

//Start = f1 [(1,2,1),(1,1,1),(2,4,5)]


// 2. Generate the first 10 element of list like: [[0],[1,1],[0,1,2,2,1,0],[0,1,2,3,3,2,1,0] ...]
l2 :: [[Int]]
l2 = take 10 [symlist e \\ e <-  [0..]]
	 where 
	 symlist :: Int -> [Int]
	 symlist n
	 | n==0 = [0]
	 | n==1 = [1,1]
	 =[0..n]++(reverse [0..n])
	 
//Start = l2
	 

// 3. Generate the first 10 powers of 2 [1,2,4,8,16 ,...]

//Start = [ 2^e \\ e <- [0..10]]


