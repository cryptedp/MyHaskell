skips:: [a]-> [[a]]
skips [] = [[]]
skips list = map (everyNth list) [1..length list]

everyNth:: [a]->Int->[a]
everyNth list n = [x | (i, x) <- zip [1..] list, i `mod` n == 0]


 
