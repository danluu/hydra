---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module NetlistTest3 where

A collection of test cases for the Hydra transformations that are used
to support structural circuit specifications, including the generation
of netlists.  The transformations are implemented using Template
Haskell, which runs in the ghc/ghci implementation of Haskell.

The tests in this module can be executed with
   ghci -fglasgow-exts -ddump-splices

> import Language.Haskell.THSyntax

> import Signal
> import Group
> import SigStruct
> import SigBool
> import SigStream
> import Format

> import CircTest3_Lbl

> main =
>   do putStrLn "Hello"
>      putStrLn "Finished..."
