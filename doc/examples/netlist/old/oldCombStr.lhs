___________________________________________________________________
	 The Hydra Computer Hardware Description Language:
	    Basic Combinational Circuits: Structural Class

			  John O'Donnell
		       University of Glasgow

   See the readme file and http://www.dcs.gla.ac.uk/~jtod/Hydra/
      Copyright (c) 2000 John O'Donnell  All Rights Reserved
___________________________________________________________________

This module defines the basic combinational logic circuits with
structural signals.

> module CombStr where
> import Signal
> import Combinator
> import CombSig

> {-  Older version
> mux1 :: Signal a => a -> a -> a -> a
> mux1 = box31 "mux1" "a""b""c" "x" f
>   where f c x y = or2 (and2 (inv c) x) (and2 c y)
> -}

> {-
> mux1 :: (Signal a, Structured a) => a -> a -> a -> a
> mux1 =
>   box31 "mux1" "ctrl" "a" "b" "x" undefined f
>   where f c x y = or2 (and2 (inv c) x) (and2 c y)
> -}

> {-
> demux1 :: (Signal a, Structured a) => a -> a -> (a,a)
> demux1 =
>   box22 "demux1" "ctrl" "a" "b" ("x","y") undefined f
>   where f c x = (and2 (inv c) x, and2 c x)
> -}

> halfAdd :: (Signal a, Structured a) => a -> a -> (a,a)
> halfAdd =
>   box22 "halfAdd" "x" "y" "sum2" ("c","s") undefined f
>   where f x y = (and2 x y, xor2 x y)

> {- fullAdd (x,y) a =
>   let c = or3 (and2 a x) (and2 a y) (and2 x y)
>       s = xor2 a (xor2 x y)
>       out = mkOut "fullAdd"
>               [S "cin" a, T2 "xy" (S "x" x) (S "y" y)]
>               (T2 "sum2" (S "cout" c) (S "s" s))
>       Tuple2 _ _ (Singleton _ _ c') (Singleton _ _ s') = out
>   in (c',s') -}

> {-
> rippleAddS :: Signal a => a -> [(a,a)] -> (a,[a])
> rippleAddS a xys =
>   let (c,ss) = mscanr fullAdd a xys
>       out = mkOut "ripple add"
>               [S "carry input" a,
>                W "data input" (zipWith f xys [0..])]
>               (T2 "ripsum"
>                 (S "cout" a)
>                 (W "sum" (zipWith g ss [0..])))
>       f (x,y) i = T2 ("[" ++ show i ++ "]") (S "x" x) (S "y" y)
>       g s i = S ("s[" ++ show i ++ "]") s
>       Tuple2 _ _ (Singleton _ _ c') (Word _ _ n ss') = out
>       ss'' = map h ss'
>       h (Singleton _ _ s) = s
>   in (c',ss'')


> {-
> main :: IO ()
> main =
>  do putStr "Test cases for module BasComb\n"
>
>     putStr "-------------------------------------------------\n"
>     putStr "Test mux1_1\n"
>     let x = mux1_1 (input "alpha" ()) (input "beta" ()) (input "gamma" ())
>     putStr ("output is " ++ show_Signal x ++ "\n")
>     let muxbox = (sigOwner (sigLoc x)) :: Box ()
>     putStr (show_Box muxbox ++ "\n")
>     let [foo,bar,baz] = boxInputs muxbox
>     putStr (show_Signal foo ++ "\n")
>     putStr (show_Signal bar ++ "\n")
>     putStr (show_Signal baz ++ "\n")
>     let [bla] = boxOutputs muxbox
>     putStr (show_Signal bla ++ "\n")
>     let cs = boxChildren muxbox
>     putStr (show (length cs) ++ " children\n")
>     putStr (show_Box (cs!!0) ++ "\n")
>     describe_Box 0 muxbox
>     putStr "test finished\n"
>
>     putStr "-------------------------------------------------\n"
>     putStr "Test mux1_2\n"
>     let x2 = mux1_2 (input "alpha" ()) (input "beta" ()) (input "gamma" ())
>     putStr ("output is " ++ show_Signal x2 ++ "\n")
>     let muxbox2 = (sigOwner (sigLoc x2)) :: Box ()
>     putStr (show_Box muxbox2 ++ "\n")
>     let [foo,bar,baz] = boxInputs muxbox2
>     putStr (show_Signal foo ++ "\n")
>     putStr (show_Signal bar ++ "\n")
>     putStr (show_Signal baz ++ "\n")
>     let [bla] = boxOutputs muxbox2
>     putStr (show_Signal bla ++ "\n")
>     let cs = boxChildren muxbox2
>     putStr (show (length cs) ++ " children\n")
>     putStr (show_Box (cs!!0) ++ "\n")
>     describe_Box 0 muxbox2
>
>     putStr "-------------------------------------------------\n"
>     putStr "Test mux1_4 with Structure!!!\n"
>     let x2 = mux1_4 (input "alpha" ()) (input "beta" ()) (input "gamma" ())
>     putStr ("output is " ++ show_Signal x2 ++ "\n")
>     let muxbox2 = (sigOwner (sigLoc x2)) :: Box ()
>     putStr (show_Box muxbox2 ++ "\n")
>     let [foo,bar,baz] = boxInputs muxbox2
>     putStr (show_Signal foo ++ "\n")
>     putStr (show_Signal bar ++ "\n")
>     putStr (show_Signal baz ++ "\n")
>     let [bla] = boxOutputs muxbox2
>     putStr (show_Signal bla ++ "\n")
>     let cs = boxChildren muxbox2
>     putStr (show (length cs) ++ " children\n")
>     putStr (show_Box (cs!!0) ++ "\n")
>
>     putStr "-------------------------------------------------\n"
>     putStr "Test mux1_4 with Bool!!!\n"
>     let x2 = mux1_4 True False True
>     foo31 mux1_4 False False False
>     foo31 mux1_4 False False True
>     foo31 mux1_4 False True  False
>     foo31 mux1_4 False True  True
>     foo31 mux1_4 True  False False
>     foo31 mux1_4 True  False True
>     foo31 mux1_4 True  True  False
>     foo31 mux1_4 True  True  True
>     putStr ("output is " ++ show x2 ++ "\n")
>
>     putStr "-------------------------------------------------\n"
>     putStr "Test halfAdd\n"
>     let (c,s) = halfAdd (input "xx" ()) (input "yy" ())
>     putStr "BasComb test cases finished\n"
>     return ()

> foo31 :: (Bool->Bool->Bool->Bool) -> Bool -> Bool -> Bool -> IO ()
> foo31 f a b c =
>   do putStr (showB a ++ showB b ++ showB c
>              ++ " ==> " ++ showB (f a b c) ++ "\n")
>      return ()

> showB :: Bool -> String
> showB False = "0"
> showB True  = "1"
> -}




