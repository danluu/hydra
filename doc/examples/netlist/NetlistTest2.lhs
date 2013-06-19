---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module NetlistTest2 where

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

> import CircTest2_Lbl

> main =
>   do putStrLn "Hello"

>      let (a,b) = halfAdd True False
>      putStrLn ("halfAdd 10 with Bool: " ++ show a ++ show b)

>      let ze = zero :: StrucSig Bool
>      let on = one :: StrucSig Bool
>      putStrLn ("zero is " ++ show (behavior ze))
>      putStrLn ("one is " ++ show (behavior on))

>      putStrLn "\nTesting halfAdd..."
>      let (c,s) = halfAdd ze ze
>      putStrLn (show (behavior c) ++ show (behavior s))
>      let (c,s) = halfAdd ze on
>      putStrLn (show (behavior c) ++ show (behavior s))
>      let (c,s) = halfAdd on ze
>      putStrLn (show (behavior c) ++ show (behavior s))
>      let (c,s) = halfAdd on on
>      putStrLn (show (behavior c) ++ show (behavior s))
>      putStrLn "Finished..."


Commented out definitions...

> {-

>      putStrLn (show (circ1 False True))
>      putStrLn (show (circ1 True  False))
>      putStrLn (show (circ1 True  True))

>      putStrLn "\nTesting circ2..."
>      putStrLn (show (circ2 False False))
>      putStrLn (show (circ2 False True))
>      putStrLn (show (circ2 True  False))
>      putStrLn (show (circ2 True  True))

>      let con0 = zero :: StrucSig Bool
>      let con1 = one  :: StrucSig Bool
>      putStrLn ("\ncon0 = " ++ show (behavior con0))
>      putStrLn ("con1 = " ++ show (behavior con1))

>      putStrLn "\ncirc1 simulation..."
>      putStrLn (show (behavior (circ1 con0 con0)))
>      putStrLn (show (behavior (circ1 con0 con1)))
>      putStrLn (show (behavior (circ1 con1 con0)))
>      putStrLn (show (behavior (circ1 con1 con1)))

>      putStrLn "\ncirc2 simulation..."
>      putStrLn (show (behavior (circ2 con0 con0)))
>      putStrLn (show (behavior (circ2 con0 con1)))
>      putStrLn (show (behavior (circ2 con1 con0)))
>      putStrLn (show (behavior (circ2 con1 con1)))

> -}
