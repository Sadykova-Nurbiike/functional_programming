module hw11 
import StdEnv 

//From slides 
:: Q = { nom :: Int , den :: Int } 
//My solution 
Q1 :: Q 
Q1 = {nom=1,den=2} 
Q2 :: Q 
Q2 = {nom=2,den=3} 
// 1. Find the cube of a fraction represented as an element of type Q. 
f1 :: Q -> Q 
f1 {nom=n,den=d} = {nom=(n^3),den=(d^3)} 
//Start = f1 Q2 
// 2. Define minQ for finding the maximum of two rational numbers. 
minQ :: Q Q -> Q 
minQ {nom=n1,den=d1} {nom=n2,den=d2} 
| n2*d1 < n1*d2 = {nom=n1,den=d1} 
={nom=n2,den=d2} 
//Start = minQ Q1 Q2 
// 3. Given a list of rational numbers, sort them in decreased order. 
isEqual :: Q Q -> Bool 
isEqual {nom=n1,den=d1} {nom=n2,den=d2} = (n2*d1 == n1*d2) 
//Start = f8 Q1 Q2 
isLess :: Q Q -> Bool 
isLess {nom=n1,den=d1} {nom=n2,den=d2} 
| n2*d1 > n1*d2 = True 
=False 
qsort :: [Q] -> [Q] 
qsort [] = [] 
qsort [c : xs] = qsort [x \\ x <- xs | (isLess x c)] ++ [c] ++ qsort [x \\ x <- xs 
                       | (not o isLess x c)||(isEqual x c)] 
                       
f3 :: [Q] -> [Q] 
f3 list = reverse (qsort list) 
Start = f3 [Q1,Q2]