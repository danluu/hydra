---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Word4Seq where

This module contains a collection of sequential circuits that operate
on 4-bit words.  These can be used as building blocks for larger
circuits.  The definitions given below are straightforward, defining
the connections for each signal explicitly.  For a more general
approach that allows arbitrary word sizes, see the WordSeq module.

> import Signal
> import BitWire
> import BitComb
> import BitSeq

> reg4 :: Clocked a => a -> [a] -> [a]
> reg4 ld [x0,x1,x2,x3] =
>   [reg1 ld x0, reg1 ld x1, reg1 ld x2, reg1 ld x3]

Meaning of op:
     0 -- no state change
     1 -- load input word x
     2 -- shift right
     3 -- shift left

> sr4 :: Clocked a => (a,a) -> a -> a -> [a] -> [a]
> sr4 op l r [x0,x1,x2,x3] = [a,b,c,d]
>   where
>      a = srb op l b x0
>      b = srb op a c x1
>      c = srb op b d x2
>      d = srb op c r x3

Several variations of the shift register, which handle the fanout in
different ways.  See the Hydra Manual section on fanout and partial
applications.

Version 1 is just the same as above.

> sr4_v1 :: Clocked a => (a,a) -> a -> a -> [a] -> [a]
> sr4_v1 op l r [x0,x1,x2,x3] = [a,b,c,d]
>   where
>      a = srb op l b x0
>      b = srb op a c x1
>      c = srb op b d x2
>      d = srb op c r x3

Version 2 uses implicit fanout expressed by defining the f box as a
partial application.

> sr4_v2 :: Clocked a => (a,a) -> a -> a -> [a] -> [a]
> sr4_v2 op l r [x0,x1,x2,x3] = [a,b,c,d]
>   where
>      f = srb op
>      a = f l b x0
>      b = f a c x1
>      c = f b d x2
>      d = f c r x3

Version 3 uses explicit fanout.

> sr4_v3 :: Clocked a => (a,a) -> a -> a -> [a] -> [a]
> sr4_v3 op l r [x0,x1,x2,x3] = [a,b,c,d]
>   where
>      (opa,opb,opc,opd) = fanout4 op
>      a = srb opa l b x0
>      b = srb opb a c x1
>      c = srb opc b d x2
>      d = srb opd c r x3



