---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Pattern where



---------------------------------------------------------------------------
			   Linear Patterns
---------------------------------------------------------------------------


> map4 f a b c d =
>   if null a || null b || null c || null d
>     then []
>     else f (head a) (head b) (head c) (head d)
>            : map4 f (tail a) (tail b) (tail c) (tail d)


Words
~~~~~

Builds a k-bit word where every bit has the value on signal x; this
is a k-bit fanout.

> -- rep k x = [x | i <- [0..k-1]]

The least significant bit of a word.

> lsb x = x!!(k-1) where k = length x

Mapping
~~~~~~~

> mapn :: (a->b) -> Int -> [a] -> [b]
> mapn f i xs
>   | i==0  = []
>   | otherwise = f (head xs) : mapn f (i-1) (tail xs)

> map2 :: (a->b->c) -> [a] -> [b] -> [c]
> map2 = zipWith


Folding
~~~~~~~

The folding patterns combine the elements of a word using a building
block f.  It corresponds to a linear computation from one end of the
word to the other, starting with an initial value a (sometimes called
an accumulator, but this is not to be confused with accumulator
registers!).  Types: a is the type of the horizontal signal, which
goes across the word from left to right, and b is the type of the
element of the word input.

> xfoldl :: (a->b->a) -> a -> [b] -> a
> xfoldl f a [] = a
> xfoldl f a (x:xs) = xfoldl f (f a x) xs

> xfoldr :: (b->a->a) -> a -> [b] -> a
> xfoldr f a [] = a
> xfoldr f a (x:xs) = f x (foldr f a xs)

Scanning
~~~~~~~~

> wscanr :: (b->a->a) -> a -> [b] -> [a]
> wscanr f a xs =
>   [foldr f a (drop (i+1) xs) | i <- [0 .. length xs -1]]

> ascanr :: (b->a->a) -> a -> [b] -> (a,[a])
> ascanr f a [] = (a,[])
> ascanr f a (x:xs) =
>   let (a',xs') = ascanr f a xs
>   in (f x a', a':xs')


> mscanr :: (a->b->(b,c)) -> b -> [a] -> (b,[c])
> mscanr f a [] = (a,[])
> mscanr f a (x:xs) =
>   let (a',ys) = mscanr f a xs
>       (a'',y) = f x a'
>   in (a'',y:ys)

> mscan :: (a->b->c->(b,a,d)) -> a -> b -> [c] -> (b,a,[d])
> mscan f a b [] = (b,a,[])
> mscan f a b (x:xs) =
>   let (b'',a',y) = f a b' x
>       (b',a'',ys) = mscan f a' b xs
>   in (b'',a'',y:ys)



---------------------------------------------------------------------------
			    Mesh Patterns
---------------------------------------------------------------------------



---------------------------------------------------------------------------
			    Tree Patterns
---------------------------------------------------------------------------


