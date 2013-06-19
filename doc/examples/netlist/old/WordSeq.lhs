---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module WordSeq where

This module defines the a collection of sequential circuits that
operate on words.  These circuits are suitable as building blocks for
larger scale designs.  They can be used at any word size, and design
patterns are used to make the specifications simple, regular and
general.

> import Signal
> import Group
> import BitWire
> import BitComb
> import BitSeq
> import WordWire
> import WordComb
> import Pattern


Register
~~~~~~~~


> reg :: Clocked a => Int -> a -> [a] -> [a]
> reg n ld x = mapn (reg1 ld) n x



Shift register
~~~~~~~~~~~~~~

> sr :: (Signal a, Clocked a)
>   => (a,a) -> a -> a -> [a] -> (a,a,[a])
> sr op l r xs = mscan (srb' op) l r xs
>   where srb' a b c d = fanout3 (srb a b c d)


Register file
~~~~~~~~~~~~~

> unbitslice2 :: [(a,b)] -> ([a],[b])
> unbitslice2 [] = ([],[])
> unbitslice2 ((x,y):zs) =
>   let (xs,ys) = unbitslice2 zs
>   in (x:xs, y:ys)

> regfile :: Clocked a => Int -> Int
>   -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a])

> regfile n k ld d sa sb x =
>    unbitslice2 [regfile1 k ld d sa sb (x!!i)  | i <- [0..n-1]]

Memory

> membit sto x =
>   let s = dff (mux1 sto s x)
>   in s

mem1 :: Clocked a => 
mem1 0 sto ps x = membit fet sto x
mem1 (k+1) sto (p:ps) =
  let m0 = mem1 k (and2 (inv p) sto) ps
      m1 = mem1 k (and2 p sto) ps
  in mux1



---------------------------------------------------------------------------
Memory

> mem1a :: Clocked a => Int -> a -> [a] -> a -> a
> mem1a 0 sto p x = reg1 sto x
> mem1a (k+1) sto (p:ps) x =
>   let (sto0,sto1) = demux1 p sto
>       m0 = mem1a k sto0 ps x
>       m1 = mem1a k sto1 ps x
>   in mux1 p m0 m1

> memw :: Clocked a => Int -> Int -> a -> [a] -> [a] -> [a]
> memw n k sto p x =
>   [mem1a k sto p (x!!i) | i <- [0..n-1]]

---------------------------------------------------------------------------

Register Transfer Machine
~~~~~~~~~~~~~~~~~~~~~~~~~

> rtm :: Clocked a => Int -> Int
>   -> a -> a -> [a] -> [a] -> [a] -> [a] -> ([a],[a],[a],a,[a])

> rtm n k ld add d sa sb x =
>   let (a,b) = regfile n k ld d sa sb y
>       y = mux1w add x s
>       (c,s) = rippleAdd zero (zipn n a b)
>   in (a,b,y,c,s)



Sequential multiplier
~~~~~~~~~~~~~~~~~~~~~

The multiplier circuit uses the sequential shift-and-add algorithm
to multiply two k-bit binary numbers, producing a 2k-bit product.
The specification is general, taking a size parameter k::Int.  The
start control signal tells the multiplier to begin computing the
product x*y, and any other multiplication in progress (if any) is
aborted.  In order to make the simulation output more interesting,
the multiplier outputs its internal register and sum values as well
as the ready signal and the product.

> mult :: Clocked a => Int
>   -> a -> [a] -> [a] -> (a,[a],[a],[a],[a])

> mult k start x y =
>   let rx = reg k one
>              (mux1w start (shr rx) x)
>       ry = reg (2*k) one
>              (mux1w start
>                 (shl ry)
>                 (fanout k zero ++ y))
>       prod = reg (2*k) one
>              (mux1w start
>                 (mux1w (lsb rx) prod s)
>                 (fanout (2*k) zero))
>       (c,s) = rippleAdd zero (bitslice2 ry prod)
>       ready = or2 (inv (any1 rx)) (inv (any1 ry))
>   in (ready,prod,rx,ry,s)


