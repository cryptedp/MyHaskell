import Data.List


countInstance :: (Eq a, Ord a) => [a] -> (a, Int)
countInstance x = (head x, length x)

countInstances :: (Eq a, Ord a) => [a] -> [(a, Int)]
countInstances x = map (countInstance) $ group$sort x

maxInstances :: [Int] -> Int
maxInstances x = maximum $ map (snd) $ countInstances x

drawRow :: [Int] -> Int-> Char
drawRow xs y
  | y `elem` xs = '*'
  | otherwise = ' '

getData :: (Eq a, Ord a) => [a] -> Int -> [a]
getData x n = [y| (y,i) <- countInstances x, i >= n]

histRow :: [Int] -> String
histRow x = map (drawRow x) [0..9]

buildHist :: [Int] -> String
buildHist x = intercalate "\n" . map (histRow) $ map (getData x) . reverse $ [1..maxInstances x]

histogram :: [Int] -> String
histogram x = buildHist x ++ "\n==========\n0123456789\n"