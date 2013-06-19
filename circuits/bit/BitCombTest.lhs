---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module BitCombTest where

This module contains examples and test cases for the definitions in
BitComb.

> import Signal
> import SigBool
> import BitComb
> import CombTools
> import Cluster

> main =
>   do putStrLn "BitCombTest"

>      putStrLn "\nmux1"
>      truthTable31 mux1
>      putStrLn "\nmux2"
>      let f [a,b,c,d,e,f] = [mux2 (a,b) c d e f] in truthTable 6 f
>      putStrLn "\ndemux1"
>      truthTable 2 (tupleword2 . wordin2 demux1)

>      putStrLn "\nhalfAdd"
>      truthTable 2 (tupleword2 . wordin2 halfAdd)
>      putStrLn "\nfullAdd"
>      let f [a,b,c] = tupleword2 (fullAdd (a,b) c) in truthTable 3 f
>      putStrLn "\nfullAdd, another approach"
>      let f [x,y,c] =
>            let (c',s) = fullAdd (x,y) c
>              in [c',s]
>        in truthTable 3 f
>      return ()
