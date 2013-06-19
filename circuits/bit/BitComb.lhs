---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module BitComb where

This module defines basic combinational logic circuits at the bit
level.

> import Signal
> import BitWire


Buffered Fanout
~~~~~~~~~~~~~~~

> fanoutbuf2 :: Signal a => a -> (a,a)
> fanoutbuf2 x = (y,y)
>   where y = buf x

> fanoutbuf3 :: Signal a => a -> (a,a,a)
> fanoutbuf3 x = (y,y,y)
>   where y = buf x

> fanoutbuf4 :: Signal a => a -> (a,a,a,a)
> fanoutbuf4 x = (y,y,y,y)
>   where y = buf x


Multiplexer
~~~~~~~~~~~

> mux1 :: Signal a => a -> a -> a -> a
> mux1 p a b = x
>   where x = or2 (and2 (inv p) a) (and2 p b)

> mux2 :: Signal a => (a,a) -> a -> a -> a -> a -> a
> mux2 (p0,p1) a b c d = x
>   where x = mux1 p0 (mux1 p1 a b) (mux1 p1 c d)

> mux22 :: Signal a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
> mux22 (p0,p1) (a0,a1) (b0,b1) (c0,c1) (d0,d1) = (x,y)
>   where x = mux2 (p0,p1) a0 b0 c0 d0
>         y = mux2 (p0,p1) a1 b1 c1 d1


Demultiplexor
~~~~~~~~~~~~~

> demux1 :: Signal a => a -> a -> (a,a)
> demux1 c x = (and2 (inv c) x, and2 c x)


Bit addition
~~~~~~~~~~~~

> halfAdd :: Signal a => a -> a -> (a,a)
> halfAdd x y = (and2 x y, xor2 x y)

> bsum, bcarry :: Signal a => (a,a) -> a -> a
> bsum (x,y) c = xor3 x y c
> bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)

> fullAdd :: Signal a => (a,a) -> a -> (a,a)
> fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)

