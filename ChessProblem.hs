import Control.Monad
import Data.List
type KnightPos = (Int,Int)
moveKnight :: KnightPos -> [KnightPos]
moveKnight(c,r) = do
	(c',r') <-[(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
               ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
               ]
	guard (c' `elem` [1..8] && r' `elem` [1..8])
	return (c',r')
in1 start = return start >>= moveKnight	
in2 start = return start >>= moveKnight >>= moveKnight	
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight
canReachIn1 :: KnightPos -> KnightPos -> Bool
canReachIn1 start end = end   `elem` in1 start
canReachIn2 :: KnightPos -> KnightPos-> Bool
canReachIn2 start end = end   `elem` in2 start
canReachIn3 :: KnightPos -> KnightPos-> Bool
canReachIn3 start end = end   `elem` in3 start

bestMove :: KnightPos -> KnightPos -> [KnightPos]
bestMove start end 
	|canReachIn1 start end = [start,end]  
	|canReachIn2 start end = [start] ++ intersect first second ++ [end]
	|canReachIn3 start end = [start] ++ intersect first third ++ intersect fourth second ++[end]
	|otherwise = []
	where 
		first = in1 start
		second = in1 end
		third = in2 end
		fourth = in2 start