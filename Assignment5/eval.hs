
import ExprT

eval:: ExprT-> Integer

eval x = case x of
	Mul x y -> eval(x)*eval(y)
	Add x y -> eval(x)+eval(y)
	Lit x -> x

