toDigits:: Integer-> [Integer]
toDigits n 
	|n<0 = []
	|n  == 0 =[]
	|otherwise = toDigits(n `div` 10) ++ [mod n 10]
	
reverseToDigits:: Integer -> [Integer]
reverseToDigits n = reverse(toDigits n)

multiplyEverySecond:: [Integer]->[Integer]
multiplyEverySecond = zipWith($) (cycle [id,(*2)])
resolveDigits:: [Integer] -> [Integer]
resolveDigits (x:xs)
	|x `div` 10 > 0 = toDigits(x) ++ resolveDigits(xs)
	|xs == [] = [x]
	|otherwise = [x] ++ resolveDigits(xs)
	
validate:: Integer ->Bool
validate n = sum(resolveDigits(multiplyEverySecond(reverseToDigits n))) `mod` 10 == 0