{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Employee


glCons:: Employee->GuestList->GuestList

glcons e (GL elist fun) = GL (elist:e) empFun e + fun

instance Monoid GuestList where
mempty = GL [] 0
mappend (GL elist1 fun1) (GL elist2 fun2) = GL (elist1++elist2) (fun1+fun2)


moreFun:: GuestList->GuestList->GuestList
morefun a@(GL elist1 fun1) b@((GL elist2 fun2)
	|fun1>fun2 = a
	|otherwise = b
	
treeFold:: (a->[b]->b)->Tree a->b
treeFold f init (Node{rootlabel=r,subForest=sf})= f r (map (treeFold f init) sf
	
