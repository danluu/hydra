---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module BitSeq where

This module defines basic sequential circuits at the bit level.

> import Signal
> import BitComb


Registers
~~~~~~~~~

> reg1 :: Clocked a => a -> a -> a
> reg1 ld x = r
>   where r = dff (mux1 ld r x)


Shift registers
~~~~~~~~~~~~~~~

> srb :: Clocked a => (a,a) -> a -> a -> a -> a
> srb op l r x = y
>   where y = dff (mux2 op y x l r)


Register files
~~~~~~~~~~~~~~

> regfile1 :: Clocked a => Int
>   -> a -> [a] -> [a] -> [a] -> a -> (a,a)

> regfile1 0 ld d sa sb x = (r,r)
>   where r = reg1 ld x

> regfile1 (k+1) ld (d:ds) (sa:sas) (sb:sbs) x = (a,b)
>   where
>     (a0,b0) = regfile1 k ld0 ds sas sbs x
>     (a1,b1) = regfile1 k ld1 ds sas sbs x
>     (ld0,ld1) = demux1 d ld
>     a = mux1 sa a0 a1
>     b = mux1 sb b0 b1




Memory
~~~~~~

> mem1
>   :: (Signal a, Clocked a)
>   => Int
>   -> a -> [a] -> [a] -> a -> a

> mem1 0 ld d sa x =
>   reg1 ld x

> mem1 (k+1) ld (d:ds) (sa:sas) x = a
>   where
>     (ld0,ld1) = demux1 d ld
>     a0 = mem1 k ld0 ds sas x
>     a1 = mem1 k ld1 ds sas x
>     a = mux1 sa a0 a1
      
