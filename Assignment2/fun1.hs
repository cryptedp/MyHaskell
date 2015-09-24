fun1 :: [Integer] -> Integer
fun1[] = 1
fun1 (x:xs)
	| even x = (x - 2)* fun1 xs
	| otherwise = fun1 xs
	
fun1' :: [Integer]-> Integer
fun1' [] = 1
fun1' x = product.map(subtract 2)$ filter (even) x	

fun2:: Integer->Integer 	
fun2 = sum . filter even . takeWhile (>1) . iterate (\n -> if even n then n `div` 2 else 3*n + 1)