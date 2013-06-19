---------------------------------------------------------------------------
      Hydra: A Functional Computer Hardware Description Language
	    --- WordWire.lhs: word-level wire networks ---
	  See the README and COPYING files and the web page:
		    www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module WordWire where

> import Signal
> import Group
> import BitWire
> import Pattern


> fanout :: Signal a => Int -> a -> [a]
> fanout k x = [x | i <- [0..k-1]]

Bit Slice organization
~~~~~~~~~~~~~~~~~~~~~~

> bitslice2 :: [a] -> [a] -> [(a,a)]
> bitslice2 = zip


Shifting
~~~~~~~~

Shift a word to the right (shr) or to the left (shl).  In both
cases, this is just a wiring pattern.  A 0 is brought in on one
side, and the bit on the other side is just thrown away.

> shr x = zero : [x!!i | i <- [0..k-2]]
>   where k = length x
> shl x = [x!!i | i <- [1..k-1]] ++ [zero]
>   where k = length x



> zipn :: Int -> [a] -> [b] -> [(a,b)]
> zipn n x y =
>   [(x!!i,y!!i) | i <- [0..n-1]]

> unzipn n xs =
>   ([fst (xs!!i) | i <- [0..n-1]],
>    [snd (xs!!i) | i <- [0..n-1]])
