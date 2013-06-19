---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Word4CombTest where

> import Signal
> import SigBool
> import BitComb
> import Word4Comb
> import CombTools

> main :: IO ()
> main =
>   do putStrLn "Word4CombTest"
>      putStrLn "winv4:"
>      truthTable 4 winv4
