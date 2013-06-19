---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module WordComb where

This module defines the a collection of combinational logic circuits
that operate on words.  These circuits are suitable as building blocks
for larger scale designs.  They can be used at any word size, and
design patterns are used to make the specifications simple, regular
and general.

> import Signal
> import Group
> import BitWire
> import BitComb
> import WordWire
> import Pattern


Word inverter: winv takes a word and inverts each of its bits

> winv :: Signal a => [a] -> [a]
> winv x = map inv x

Duplicating a bit to form a word: fanoutbuf takes a wordsize n and a
signal x, and produces a word of size n each of whose bits takes the
value of x.

> fanoutbuf :: Signal a => Int -> a -> [a]
> fanoutbuf n x = [x | i <- [0..n-1]]

Representing a boolean bit as a word: boolword takes a bit x, and pads
it to the left with 0s to form a word.  If the input x is False (0),
the result is the integer 0 (i.e. n 0-bits), and if x is True (1) the
result is the integer 1 (rightmost bit is 1, all others are 0).

> boolword :: Signal a => Int -> a -> [a]
> boolword n x = fanoutbuf (n-1) zero ++ [x]



> orw :: Signal a => [a] -> a
> orw = foldl or2 zero

> demux1w :: Signal a => [a] -> a -> [a]
> demux1w [c0] x =
>   let (a0,a1) = demux1 c0 x
>   in [a0,a1]

> demux2w :: Signal a => [a] -> a -> [a]
> demux2w [c0,c1] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux1w [c1] a0
>       w1 = demux1w [c1] a1
>   in w0++w1

> demux3w :: Signal a => [a] -> a -> [a]
> demux3w [c0,c1,c2] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux2w [c1,c2] a0
>       w1 = demux2w [c1,c2] a1
>   in w0++w1

> demux4w :: Signal a => [a] -> a -> [a]
> demux4w [c0,c1,c2,c3] x =
>   let (a0,a1) = demux1 c0 x
>       w0 = demux3w [c1,c2,c3] a0
>       w1 = demux3w [c1,c2,c3] a1
>   in w0++w1




> mux2w cc = map4 (mux2 cc)


And/Or over a word
~~~~~~~~~~~~~~~~~~

Determine whether there exists a 1 in a word, or whether all the
bits are 0.  A tree fold can do this in log time, but for
simplicity this is just a linear time fold.

> any0, any1, all0, all1 :: Signal a => [a] -> a
> any1 = foldl or2 zero
> all1 = foldl and2 one
> all0 xs = inv (any1 xs)
> any0 xs = inv (all1 xs)


Multiplexor
~~~~~~~~~~~

> mux1w :: Signal a => a -> [a] -> [a] -> [a]
> mux1w c x y = map2 (mux1 c) x y


Demultiplexor
~~~~~~~~~~~~~


Ripple carry addition
~~~~~~~~~~~~~~~~~~~~~

> rippleAdd :: Signal a => a -> [(a,a)] -> (a,[a])
> rippleAdd = mscanr fullAdd



Two's complement addition and subtraction
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> addSub :: Signal a => a -> [(a,a)] -> (a,[a])
> addSub sub xy = rippleAdd sub (map f xy)
>   where f (x,y) = (x, xor2 sub y)


Binary comparitor
~~~~~~~~~~~~~~~~~

> cmp1 :: Signal a => (a,a,a) -> (a,a) -> (a,a,a)
> cmp1 (lt,eq,gt) (x,y) =
>   (or2 lt (and3 eq (inv x) y),
>    and2 eq (inv (xor2 x y)),
>    or2 gt (and3 eq x (inv y))
>   )

> rippleCmp :: Signal a => [(a,a)] -> (a,a,a)
> rippleCmp = foldl cmp1 (zero,one,zero)


