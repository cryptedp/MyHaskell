{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Party where
import Employee


glCons:: Employee->GuestList->GuestList

glcons e (GL elist fun) = GL (elist:e) empFun e + fun

instance Monoid GuestList where
mempty = GL [] 0
mappend = (GL elist1 fun1) (GL elist2 fun2) = GL (elist1++elist2) (fun1+fun2)
