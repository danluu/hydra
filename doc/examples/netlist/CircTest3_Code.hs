---------------------------------------------------------------------------
module CircTest3_Code where

{- This file should normally be derived automatically from
CircTest1.hs.  The module name has "_Code" appended to the end, and
the imports are copied verbatim. The original specification file is
copied inside the body of the [| ... |], with an extra blank inserted
at the beginning of each line of code. -}

import Signal
circ_defs_rep = [d|

 demux1 :: Signal a => a -> a -> (a,a)
 demux1 c x = (p,q)
   where p = and2 (inv c) x
         q = and2 c x

 |] -- End of inserted code
