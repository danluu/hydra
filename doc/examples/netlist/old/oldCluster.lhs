> module CluClass where

> import Signal


> class Clueful a where
>   showCluf :: a -> String
>   repClu :: (Maybe String) -> a -> Cluster Bool ()
>   unrepClu :: Cluster Bool () -> a


> instance Clueful Bool where
>   showCluf x = "<" ++ show x ++ ">"
>   repClu s x = Singleton s (error "ba") x
>   unrepClu (Singleton _ _ x) = x

> instance (Clueful a, Clueful b) => Clueful (a,b) where
>   showCluf (x,y) = "(" ++ showCluf x ++ "," ++ showCluf y ++ ")"
>   repClu s (x,y) = Tuple2 s (error "ba") (repClu Nothing x)
>                         (repClu Nothing  y)
>   unrepClu (Tuple2 _ _ x y) = (unrepClu x, unrepClu y)

> t1 = showCluf (True,False)
> t2 = repClu (Just "hello") (True,False)
> t2' = unrepClu t2 :: (Bool,Bool)


> class Clueful2 a b where
>   bla :: a -> b -> b
> --  showCluf2 :: b -> String
>   repClu2 :: (Maybe String) -> b -> Cluster a ()
>   unrepClu2 :: Cluster a () -> b


> instance Clueful2 Bool Bool where
> --  showCluf2 x = "<" ++ show x ++ ">"
>   repClu2 s x = Singleton s (error "ba") x
>   unrepClu2 (Singleton _ _ x) = x


> instance (Clueful2 a b, Clueful2 a c) => Clueful2 a (b,c) where
> --  showCluf2 (x,y) = "(" ++ showCluf2 x ++ "," ++ showCluf2 y ++ ")"
>   repClu2 s (x,y) = Tuple2 s (error "ba") (repClu2 Nothing x)
>                         (repClu2 Nothing  y)
>   unrepClu2 (Tuple2 _ _ x y) = (unrepClu2 x, unrepClu2 y)




> u1 = repClu2 (Just "bye") (False,True) :: Cluster Bool ()
> u2 = unrepClu2 u1 :: (Bool,Bool)
