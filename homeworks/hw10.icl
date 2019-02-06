module hw10
import StdEnv


// For ex 1,2,3 use the Tree Int type defined in the class.

:: Tree a = Node a (Tree a) (Tree a)
          | Leaf
aTree :: Tree Int
aTree = Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 2 Leaf Leaf)

// 1. Check if a number is a node of a tree.
isNode :: (Tree Int) Int -> Bool
isNode Leaf n = False
isNode (Node x le ri) n 
|x==n = True
= (isNode le n) || (isNode ri n)

//Start = isNode aTree 9

// 2. Count how many times is a node in a tree.
nrNode :: (Tree Int) Int -> Int
nrNode Leaf n = 0
nrNode (Node x le ri) n 
|x==n = 1 + (nrNode le n) + (nrNode ri n)
= (nrNode le n) + (nrNode ri n)

//Start = nrNode aTree 2

// 3. If a number is an a tree, then give the list of its children in the form of (left child, right child).
//if the children are leaves then (0,0) are its children.

child :: (Tree Int) Int -> [(Int, Int)]
child (Node x Leaf Leaf) n  
| x==n = [(0,0)]
=[]

child (Node x Leaf (Node y le ri)) n  
| x==n = [(0,y)] ++ (child (Node y le ri) n)
=(child (Node y le ri) n)

child (Node x (Node y le ri) Leaf) n  
| x==n = [(y,0)] ++ (child (Node y le ri) n)
=(child (Node y le ri) n)

child (Node x (Node y le1 ri1) (Node z le2 ri2)) n
|x==n = [(y,z)] ++ (child (Node y le1 ri1) n)  ++ (child (Node z le2 ri2) n) 
=(child (Node y le1 ri1) n)  ++ (child (Node z le2 ri2) n) 

// eg. for aTree = Node 4 (Node 2 (Node 1 Leaf Leaf) (Node 3 Leaf Leaf)) (Node 2 Leaf Leaf)
// has 2 as node two times, and the result for Start = child aTree 2 is [(1,3),(0,0)]

//Start = child aTree 2
