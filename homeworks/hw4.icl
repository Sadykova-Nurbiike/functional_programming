module hw4
import StdEnv

givesdaysofmonths :: Int -> [Int]
givesdaysofmonths x
| (x rem 4) > 0 = [31,28,31,30,31,30,31,31,30,31,30,31]
| (x rem 100) > 0 = [31,29,31,30,31,30,31,31,30,31,30,31]
| (x rem 400) > 0 = [31,28,31,30,31,30,31,31,30,31,30,31]
| otherwise = [31,29,31,30,31,30,31,31,30,31,30,31]

countsdays::[Int] -> Int
countsdays x = foldl (+) (x!!2) (take ((x!!1)-1) (givesdaysofmonths (x!!0))) 

final :: [Int] [Int] -> Int
final x y
| countsdays x == countsdays [hd x : tl y] = 0
| countsdays x > countsdays [hd x : tl y] = countsdays [hd x : tl y] - countsdays x + 365
| countsdays x < countsdays [hd x : tl y] = countsdays [hd x : tl y] - countsdays x

Start= final [2018,3,12] [2000,3,4]