---------------------------------------------------------------------------
module CircTest2_Code where

{- This file should normally be derived automatically from
CircTest1.hs.  The module name has "_Code" appended to the end, and
the imports are copied verbatim. The original specification file is
copied inside the body of the [| ... |], with an extra blank inserted
at the beginning of each line of code. -}

import Signal
circ_defs_rep = [d|

---------------------------------------------------------------------------
--			  General test cases
---------------------------------------------------------------------------

 simplest_circ :: Signal a => a -> a
 simplest_circ x = x

 simple_circ :: Signal a => a -> a
 simple_circ x = y
   where y = inv x

 circ_locals :: Signal a => a -> a -> a -> a
 circ_locals a b c = y
   where x = and2 a b
         y = or2 a x

 circ_complex_tup_inp :: Signal a => (a,a) -> (a,(a,a),a) -> a
 circ_complex_tup_inp (a,b) (c,(d,e),f) = x
   where x = or3 (and2 a b) (and2 c d) (xor2 e f)

-- circ_word_inp :: Signal a => [a] -> a
-- circ_word_inp [a,b,c] = and3 a b c

 circ_complex_rhs :: Signal a => a -> a -> a
 circ_complex_rhs a b = y
   where y = or2 (and2 a b) (xor2 b a)

 circ_complex_out :: Signal a => a -> a -> a
 circ_complex_out a b = y
   where y = or2 (and2 a b) (xor2 b a)

---------------------------------------------------------------------------
--			     Bit addition
---------------------------------------------------------------------------

 halfAdd :: Signal a => a -> a -> (a,a)
 halfAdd x y = (c,s)
   where c = and2 x y
         s = xor2 x y

 bsum, bcarry :: Signal a => (a,a) -> a -> a
 bsum (x,y) c = s
   where s = xor3 x y c
 bcarry (x,y) c = c
   where c = or3 (and2 x y) (and2 x c) (and2 y c)


 fullAdd :: Signal a => (a,a) -> a -> (a,a)
 fullAdd (x,y) c = (c',s)
   where c' = bcarry (x,y) c
         s = bsum (x,y) c

---------------------------------------------------------------------------
--				Fanout
---------------------------------------------------------------------------

 fanoutbuf2 :: Signal a => a -> (a,a)
 fanoutbuf2 x = (y,y)
   where y = buf x

 fanoutbuf3 :: Signal a => a -> (a,a,a)
 fanoutbuf3 x = (y,y,y)
   where y = buf x

 fanoutbuf4 :: Signal a => a -> (a,a,a,a)
 fanoutbuf4 x = (y,y,y,y)
   where y = buf x

---------------------------------------------------------------------------
--			     Multiplexors
---------------------------------------------------------------------------

 mux1 :: Signal a => a -> a -> a -> a
 mux1 c x y = p
   where p = or2 (and2 (inv c) x) (and2 c y)



 mux2 :: Signal a => (a,a) -> a -> a -> a -> a -> a
 mux2 (c,d) p q w r = x
   where x = mux1 c (mux1 d p q) (mux1 d w r)

 mux22 :: Signal a => (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a) -> (a,a)
 mux22 (a,b) (w0,w1) (x0,x1) (y0,y1) (z0,z1) = (p,q)
  where p = mux2 (a,b) w0 x0 y0 z0
        q = mux2 (a,b) w1 x1 y1 z1

 demux1 :: Signal a => a -> a -> (a,a)
 demux1 c x = (p,q)
   where p = and2 (inv c) x
         q = and2 c x


---------------------------------------------------------------------------
--			      Registers
---------------------------------------------------------------------------

 reg1 :: Clocked a => a -> a -> a
 reg1 ld x = r
   where r = dff (mux1 ld r x)

---------------------------------------------------------------------------
 |] -- End of inserted code
---------------------------------------------------------------------------
