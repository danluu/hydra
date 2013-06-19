-----------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
			  Stream Signals

Copyright (c) 2001 John O'Donnell.  See the README file for general
information, COPYING for the full copyright, and the web page for the
latest version: http://www.dcs.gla.ac.uk/~jtod/Hydra/
-----------------------------------------------------------------------

> module SigStream where
> import Signal


Representation of Streams
~~~~~~~~~~~~~~~~~~~~~~~~~

> data Stream a = Cycle a (Stream a)

> listStream :: [a] -> Stream a
> listStream [] = error "no more stream data from list"
> listStream (x:xs) = Cycle x (listStream xs)



> current :: Stream a -> a
> current (Cycle x xs) = x

> future :: Stream a -> Stream a
> future (Cycle x xs) = xs

> forever :: a -> Stream a
> forever x = Cycle x (forever x)

> instance (Show a, Static a) => Show (Stream a) where
>   show (Cycle x xs) = showSig x ++ "," ++ show xs


Stream Signal Operations
~~~~~~~~~~~~~~~~~~~~~~~~

> instance Static a => Signal (Stream a) where
>   zero = forever zero
>   one  = forever one
>   buf  = mapStream buf
>   inv  = mapStream inv
>   and2 = map2Stream and2
>   or2  = map2Stream or2
>   nand2 = map2Stream and2
>   nor2  = map2Stream or2
>   xor2  = map2Stream xor2
>   xnor2 = map2Stream xnor2
>   and3  = map3Stream and3
>   or3   = map3Stream or3
>   nand3 = map3Stream and3
>   nor3  = map3Stream or3
>   xor3  = map3Stream xor3
>   xnor3 = map3Stream xnor3
>   and4  = map4Stream and4
>   or4   = map4Stream or4
>   nand4 = map4Stream and4
>   nor4  = map4Stream or4
>   xor4  = map4Stream xor4
>   xnor4 = map4Stream xnor4
>   alias x _ _ _ = x

> mapStream :: (a->a) -> Stream a -> Stream a
> mapStream f x
>   = Cycle
>       (f (current x))
>       (mapStream f (future x))

> map2Stream :: (a->b->c) -> Stream a -> Stream b -> Stream c
> map2Stream f x y =
>   Cycle
>     (f (current x) (current y))
>     (map2Stream f (future x) (future y))

> map3Stream :: (a->b->c->d) -> Stream a -> Stream b -> Stream c
>         -> Stream d
> map3Stream f x y z =
>   Cycle
>     (f (current x) (current y) (current z))
>     (map3Stream f (future x) (future y) (future z))

> map4Stream :: (a->b->c->d->e) -> Stream a -> Stream b -> Stream c
>   -> Stream d -> Stream e
> map4Stream f w x y z =
>   Cycle
>     (f (current w) (current x) (current y) (current z))
>     (map4Stream f (future w) (future x) (future y) (future z))


Streams represent clocked signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> instance Static a => Clocked (Stream a) where
>   dff x = Cycle zero x

> intsSig :: Static a => [Int] -> Stream a
> intsSig (x:xs) = Cycle (intSig x) (intsSig xs)

..................................................
Junk...

> {-
>   box11 = box11Stream
>   box21 = box21Stream
>   box31 = box31Stream
>   box41 = box41Stream
>   box22 = box22Stream
>  --  mkOut = mkOut
> -}

> {- There is something to work out here! ???
> box31Stream
>   :: (Signal a)
>   => String -> String -> String -> String -> String
>   -> (Stream a -> Stream a -> Stream a -> Stream a)
>   -> Stream a -> Stream a -> Stream a -> Stream a  
> -- box31Stream a b c d e f p q r = box31 a b c d e f p q r
>     -- the def above  fails!
> -- box31Stream a b c d e f p q r = f p q r -- works
> box31Stream a b c d e f = f  -- works
> -}

>{- box11Stream _ _ _ f = f
> box21Stream _ _ _ _ f = f
> box31Stream _ _ _ _ _ f = f
> box41Stream _ _ _ _ _ _ f = f
> box22Stream _ _ _ _ _ f = f -}
