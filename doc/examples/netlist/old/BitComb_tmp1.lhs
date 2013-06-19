> module BitComb_tmp1 where
> import Signal

> circ_defs_rep = [d|

from BitWire...

>  fanout2 :: a -> (a,a)
>  fanout2 x = (x,x)

>  fanout3 :: a -> (a,a,a)
>  fanout3 x = (x,x,x)

>  fanout4 :: a -> (a,a,a,a)
>  fanout4 x = (x,x,x,x)


The BitComb.lhs file is copied here, with an extra blank inserted
after the > beginning each line of code.

>  circ1 a b c = (x,y)
>    where x = and2 a b
>          y = or2 b c


Beginning of inserted code
~~~~~~~~~~~~~~~~~~~~~~~~~~

>  -- fanoutbuf2 :: Signal a => a -> (a,a)
>  fanoutbuf2 x = (y,y)
>    where y = buf x

>  -- fanoutbuf3 :: Signal a => a -> (a,a,a)
>  fanoutbuf3 x = (y,y,y)
>    where y = buf x

>  -- fanoutbuf4 :: Signal a => a -> (a,a,a,a)
>  fanoutbuf4 x = (y,y,y,y)
>    where y = buf x

Multiplexer
~~~~~~~~~~~

>  -- mux1 :: Signal a => a -> a -> a -> a
>  mux1 c x y = or2 (and2 (inv c) x) (and2 c y)

>  -- mux2 :: Signal a => (a,a) -> a -> a -> a -> a -> a
>  mux2 (c,d) p q w r =
>    mux1 c (mux1 d p q) (mux1 d w r)

>  -- mux22 :: Signal a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
>  mux22 (a,b) (w0,w1) (x0,x1) (y0,y1) (z0,z1) =
>    (mux2 (a,b) w0 x0 y0 z0,
>     mux2 (a,b) w1 x1 y1 z1)

Demultiplexor
~~~~~~~~~~~~~

>  -- demux1 :: Signal a => a -> a -> (a,a)
>  demux1 c x = (and2 (inv c) x, and2 c x)


Bit addition
~~~~~~~~~~~~

>  -- myhalfAdd :: Signal a => a -> a -> a   -- triggers ghc bug
> --  myhalfAdd a b = (and2 a b, xor2 a b)

>  -- halfAdd :: Signal a => a -> a -> (a,a)
>  halfAdd x y = (and2 x y, xor2 x y)

>  -- bsum, bcarry :: Signal a => (a,a) -> a -> a
>  bsum (x,y) c = xor3 x y c
>  bcarry (x,y) c = or3 (and2 x y) (and2 x c) (and2 y c)

>  -- fullAdd :: Signal a => (a,a) -> a -> (a,a)
>  fullAdd (x,y) c = (bcarry (x,y) c, bsum (x,y) c)


End of inserted code
~~~~~~~~~~~~~~~~~~~~

>  |]


Definitions that are commented out...



> -- circ2 :: Signal a => a -> a -> a   -- ghc bug
> -- circ2 p q = and2 (xor2 p q) (circ1 (p,q) (p,(p,q),q))

> --    circ3 :: Signal a => a -> a -> a   -- triggers ghc bug
> --      circ3 (a,b) (d,(e,f),g) = or2 (and2 a b) (or2 b a)
> --        where x = or2 b a
> --              y = and2 x a
> --               z = "abc"
> --               i = 123 :: Int

> {-



> -- reg1 :: Clocked a => a -> a -> a
> --   reg1 ld x = r
> --     where r = dff (mux1 ld r x)

> -}
