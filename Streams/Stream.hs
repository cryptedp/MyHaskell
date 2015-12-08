data Stream a = Cons a(Stream a)

streamToList:: Stream a > [a]
streamToList(Cons x s) = x:streamToList s

instance Show a => Show (Stream a) where
show (Cons x s ) = show x ++ ","++( concat.map(\s->show s++",").take 30.streamToList) s

streamrepeat:: a-> Stream a
streamrepeat a = Cons a $ streamrepeat a

streamMap:: (a->b)->Stream a->Stream b
streamMap f Cons a s =Cons(f a) (streamMap f s)

streamFromSeed:: (a->a) -> a -> Stream a
streamFromSeed f seed = Cons(seed)(streamFromSeed f(f seed))

nats:: Stream Integer
nats = streamFromSeed (\x-> x+1)0