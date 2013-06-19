---------------------------------------------------------------------------
The Hydra Computer Hardware Description Language
See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module BitWire where

This module defines wiring patterns---pure connections that don't
contain any active components---at the bit level.

> import Signal


Fanout patterns
~~~~~~~~~~~~~~~

> fanout2 :: a -> (a,a)
> fanout2 x = (x,x)

> fanout3 :: a -> (a,a,a)
> fanout3 x = (x,x,x)

> fanout4 :: a -> (a,a,a,a)
> fanout4 x = (x,x,x,x)
