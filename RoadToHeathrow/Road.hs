data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)  
type RoadSystem = [Section]
heathrowToLondon :: RoadSystem
heathrowToLondon = [Section 50 10 30, 5 90 20, 40 2 25, 10 8 0]
data Label = A | B | C deriving (Show)  
type Path = [(Label, Int)]  
roadStep :: (Path, Path) -> Section -> (Path, Path)  
roadStep (pathA, pathB) (Section a b c) =  
	let priceA = sum $ map snd pathA  
		priceB = sum $ map snd pathB  
		forwardPriceToA = priceA + a  
		crossPriceToA = priceB + b + c  
		forwardPriceToB = priceB + b  
		crossPriceToB = priceA + a + c  
		newPathToA = if forwardPriceToA <= crossPriceToA  
						then (A,a):pathA  
						else (C,c):(B,b):pathB  
		newPathToB = if forwardPriceToB <= crossPriceToB  
						then (B,b):pathB  
						else (C,c):(A,a):pathA  
	in  (newPathToA, newPathToB)  
	
optimalPath :: RoadSystem -> Path  
optimalPath roadSystem = 
	let (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem  
	in  if sum (map snd bestAPath) <= sum (map snd bestBPath)  
			then reverse bestAPath  
			else reverse bestBPath  