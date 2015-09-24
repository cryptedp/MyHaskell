data Tree a = Leaf|Node Integer (Tree a) a (Tree a) deriving (Show,Eq)

foldTree:: [a]->Tree a




foldTree = foldr(\x tree -> insertInTree x tree) Leaf

insertInTree:: a->Tree a -> Tree a
insertInTree x Leaf = Node 0 (Leaf) x (Leaf)
insertInTree x (Node n t1 val t2) 
    | heightRight < heightLeft  = Node  n (insertInTree x t1) val t2 
    | heightRight > heightLeft  = Node  n    t1 val t2n 
    | otherwise = Node (h+1) t1 val t2n  
  where heightRight  = heightTree t1
        heightLeft  = heightTree t2
        t2n = insertInTree x t2
        h   = heightTree t2n  
	
heightTree:: Tree a -> Integer
heightTree Leaf = 0
heightTree (Node n t1 val t2) = n

xor :: Bool -> Bool -> Bool
xor True p = not p
xor False p = p
