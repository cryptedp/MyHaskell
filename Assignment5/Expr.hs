

import Parser
import ExprT
import StackVM
import VarExprT



class Expr a where
    add, mul :: a -> a -> a
    lit :: Integer -> a

instance Expr ExprT where
    add a b = ExprT.Add a b
    mul x y = ExprT.Mul x y
    lit n   = ExprT.Lit n

instance Expr Integer where
    add a b = a + b
    mul x y = x * y
    lit n   = n

instance Expr Bool where
    add a b = a || b
    mul x y = x && y
    lit n
        | n <= 0 = False
        | otherwise = True

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
	
	
class HasVars a where
  var :: String -> a


instance Expr VarExprT where
	lit n = VarExprT.Lit n
	add a b = VarExprT.Add a b
	mul x y = VarExprT.Mul x y
	
instance HasVars VarExprT where
  var s = Var s
  
instance Expr (M.Map String Integer -> Maybe Integer) where
  add x y m = liftM2 (+) (x m) (y m)
  mul x y m = liftM2 (*) (x m) (y m)
  lit n _   = Just n

instance HasVars (M.Map String Integer -> Maybe Integer) where
  var = M.lookup

withVars :: [(String, Integer)]
         -> (M.Map String Integer -> Maybe Integer)
         -> Maybe Integer
withVars vs expr = expr $ M.fromList vs
	
