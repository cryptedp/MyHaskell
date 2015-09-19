localMaxima:: [Integer]->[Integer]
localMaxima [] = []
localMaxima (x:xs) 
	|length (x:xs) < 3 = []
	|maximum(x:take 2 xs) == head xs = [head xs] ++ localMaxima (xs)	
	|otherwise = localMaxima(xs)
	
