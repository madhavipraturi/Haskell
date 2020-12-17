import Data.List
import System.IO


data Tree a = Leaf a | Node (Tree a) (Tree a) deriving (Show)


--generalTree :: Tree Integer  
--generalTree =   
 --   Node 51  
   --     (Leaf 10)  
     --   (Node 17  
       --     (Leaf 8 )  
         --   (Leaf 42)  
       -- )
		
--data Tree a = Leaf a | Node a (Tree a) (Tree a) 

generalTree :: Tree a 
generalTree =   
    Node   
        (Leaf 11)  
        (Node  
            (Node  
            (Leaf 6)
			(Leaf 3)
			)
			(Leaf 9)
        )	

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)


basicTree :: Tree Integer  
basicTree =   
    Node 51  
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
		


treeRow :: Int -> Tree a -> [a]
treeRow 0 (Node a _ _) = [a]
treeRow n (Empty) = []
treeRow n (Node a t1 t2) = merge (treeRow (n-1) t1) (treeRow (n-1) t2)

mirror :: Tree a -> Tree a
mirror (Node a _ _) = (Node a _ _)
mirror (Node a t1 t2) = Node a (mirror t2) (mirror t1) 

breadthFirst :: (Tree a) -> [a]
--breadthFirst (Node a _ _) = [a]
breadthFirst (Empty) = []
breadthFirst (Node a t1 t2) = a : merge (breadthFirst t1) (breadthFirst t2)

merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys

--allLevels :: Tree a -> [[a]]
--allLevels (Leaf a) = [a] : []
--allLevels (Node a t1 t2) = [a] : merge (allLevels t1) (allLevels t2)



