---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Word4Comb where

This module contains a collection of combinational circuits that
operate on 4-bit words.  These can be used as building blocks for
larger circuits.  The definitions given below are straightforward,
defining the connections for each signal explicitly.  For a more
general approach that allows arbitrary word sizes, see the WordComb
module.

> import Signal
> import BitWire
> import BitComb

> winv4 :: Signal a => [a] -> [a]
> winv4 [x0,x1,x2,x3]
>   = [inv x0, inv x1, inv x2, inv x3]

> rippleAdd4 :: Signal a => a -> [(a,a)] -> (a,[a])
> rippleAdd4 cin [(x0,y0),(x1,y1),(x2,y2),(x3,y3)]
>   = (c0, [s0,s1,s2,s3])
>   where
>     (c0,s0) = fullAdd (x0,y0) c1
>     (c1,s1) = fullAdd (x1,y1) c2
>     (c2,s2) = fullAdd (x2,y2) c3
>     (c3,s3) = fullAdd (x3,y3) cin

