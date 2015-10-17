data JoinList m a = Empty
| Single m a
| Append m (JoinList m a) (JoinList m a)
deriving (Eq, Show)

(+++):: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
Single m1 a `+++` Single m2 b = Append  m1 <> m2(Single m1 a) (Single m2 b)
Single m1 a `+++` Append m2 (JoinList m2 b)(JoinList m3 c )= Append m1<>m2 (Single m1 a)( Append m2 (JoinList m2 b)(JoinList m3 c ))
Append m2 (JoinList m2 b)(JoinList m3 c )`+++`Single m1 a = Append m1<>m2 (Single m1 a)( Append m2 (JoinList m2 b)(JoinList m3 c ))
Append m1 a (JoinList m2 b)(JoinList m3 c) `+++` Append m4 d (JoinList m5 e)(JoinList m6 7) = Append m1<>m4 (Append m1 a (JoinList m2 b)(JoinList m3 c))(Append m4 d (JoinList m5 e)(JoinList m6 7))