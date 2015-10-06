import ExprT
import Parser

eval:: ExprT-> Integer

eval x = case x of
	Mul x y -> eval(x)*eval(y)
	Add x y -> eval(x)+eval(y)
	Lit x -> x

evalStr:: String -> Maybe Integer
evalStr s = do
	e <- parseExp Lit Add Mul s
	return $eval e
	
class Expr a where
    add, mul :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    add a b = Add a b
    mul x y = Mul x y
    lit n   = Lit n

reify:: ExprT->ExprT
reify = id

instance Expr Integer where
add a b = a+b
mul x y = x*y
lit n = n

instance Expr Bool where
add a b = a||b
mul x y = x && y
lit n 
	|n<=0 = False
	|otherwise = True

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7   Integer deriving (Eq, Show)

instance Expr MinMax where
    add (MinMax a) (MinMax b) = MinMax (max a b)
    mul (MinMax x) (MinMax y) = MinMax (min x y)
    lit n   = MinMax n

instance Expr Mod7 where
    add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
    mul (Mod7 x) (Mod7 y) = Mod7 ((x * y) `mod` 7)
    lit n   = Mod7 (n `mod` 7)
