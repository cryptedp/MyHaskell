xor':: [Bool]-> Bool
xor' = foldl(\x y -> xor x y) False




xor :: Bool -> Bool -> Bool
xor True p = not p
xor False p = p

map'::  (a->b)->[a]->[b]

map' f = foldr(\x acc-> f x : acc)[]
