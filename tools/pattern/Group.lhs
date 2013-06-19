---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Group where



> field :: [a] -> Int -> Int -> [a]
> field x a n = [x!!i | i <- [a .. a+n-1]]

Conversions from tuples to words
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> tupleword0 :: () -> [a]
> tupleword0 () = []

> tupleword1 :: a -> [a]
> tupleword1 a = [a]

> tupleword2 :: (a,a) -> [a]
> tupleword2 (a,b) = [a,b]

> tupleword3 :: (a,a,a) -> [a]
> tupleword3 (a,b,c) = [a,b,c]

> tupleword4 :: (a,a,a,a) -> [a]
> tupleword4 (a,b,c,d) = [a,b,c,d]


Conversions from words to tuples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> wordtuple0 :: [a] -> ()
> wordtuple0 [] = ()

> wordtuple1 :: [a] -> (a)
> wordtuple1 [a] = a

> wordtuple2 :: [a] -> (a,a)
> wordtuple2 [a,b] = (a,b)

> wordtuple3 :: [a] -> (a,a,a)
> wordtuple3 [a,b,c] = (a,b,c)

> wordtuple4 :: [a] -> (a,a,a,a)
> wordtuple4 [a,b,c,d] = (a,b,c,d)


Gathering component inputs into tuples
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> tuplein1 :: (a->b) -> (a->b)
> tuplein1 = id

> tuplein2 :: (a->b->c) -> ((a,b)->c)
> tuplein2 f (a,b) = f a b

> tuplein3 :: (a->b->c->d) -> ((a,b,c)->d)
> tuplein3 f (a,b,c) = f a b c

> tuplein4 :: (a->b->c->d->e) -> ((a,b,c,d)->e)
> tuplein4 f (a,b,c,d) = f a b c d


Gathering component inputs into words
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> wordin1 :: (a->b) -> ([a]->b)
> wordin1 f = tuplein1 f . wordtuple1

> wordin2 :: (a->a->b) -> ([a]->b)
> wordin2 f = tuplein2 f . wordtuple2

> wordin3 :: (a->a->a->b) -> ([a]->b)
> wordin3 f = tuplein3 f . wordtuple3

> wordin4 :: (a->a->a->a->b) -> ([a]->b)
> wordin4 f = tuplein4 f . wordtuple4
