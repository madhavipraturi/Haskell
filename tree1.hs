import System.IO


-- 2.1 (a) Distribute function	
distribute :: a ->[a] ->[[a]]
distribute x ys = (x:ys) : final x ys (length ys)

final :: a -> [a] -> Int -> [[a]]
final x ys 0 = []
final x ys n= insertAt x ys n : final x ys (n-1)

--Inserts the element x at all possible positions within the list xs
insertAt :: a -> [a] -> Int -> [a]
insertAt x ys 0 = x:ys
insertAt x (y:ys) n = y:insertAt x ys (n-1)

--Returns the list of all possible permutations of the elements of the input list
buildPerm :: [a] -> Int -> [[a]]
buildPerm xs 0 = []
--Calls "distribute" with head element and tail of input list. 
--It then inserts the head at end of the list and calls "distribute" again with current head element.
buildPerm xs n = (distribute (head xs) (tail xs)) ++ (buildPerm ((tail xs) ++ [head xs]) (n-1))

-- 2.1 (b) Permutations function
permutations :: [a] -> [[a]]
permutations xs = buildPerm xs (length xs)

{- 2.2 (a)  Haskell value of type Tree Int that represents the binary tree, with given definition

data Tree a = Leaf a | Node a (Tree a) (Tree a) deriving (Show)


generalTree :: Tree Integer  
generalTree =   
    Node 51  
        (Leaf 10)  
        (Node 17  
            (Leaf 8 )  
            (Leaf 42)  
        )
		
-}

--2.2 (b) treeRow function on a Tree 
data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

--Builds a basic tree
basicTree :: Tree Integer  
basicTree =   
    (Node 51  
        (Node 10
			(Empty)
			(Empty)
		)
        (Node 17  
            (Node 8 
				(Empty)
				(Empty)
			)  
            (Node 42
				(Empty)
				(Empty)
			)  
        )  
	)
--Returns the values nth row of tree t in order, from left to right		
treeRow :: Int -> Tree a -> [a]
treeRow 0 (Node a _ _) = [a]
treeRow n (Empty) = []
treeRow n (Node a t1 t2) =  (treeRow (n-1) t1) ++ (treeRow (n-1) t2)

--2.2 (c) breadthFirst function: collapses the values from a tree into a list using a breadth-first strategy
breadthFirst :: (Tree a) -> [a]
breadthFirst (Empty) = []
breadthFirst (Node a t1 t2) = a : merge (breadthFirst t1) (breadthFirst t2)

--function to merge two lists into a single list 
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) =  x: y : merge  xs ys










