---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module WordCombTest where

> import WordComb
> import Group
> import WordWire
> import Signal
> import SigStream
> import SigBool
> import Format


Addition circuits
~~~~~~~~~~~~~~~~~

Test an adder circuit named addcirc entering an expression like the
following:

  sim_adder rippleAdd4  4 add_input1
  sim_adder rippleAdd   4 add_input1
  sim_adder rippleAdd  16 add_input1

Suitable circuits to test are
  rippleAdd4  (in Word4Comb)
  rippleAdd   (in WordComb)

Test data for adder
~~~~~~~~~~~~~~~~~~~

> add_input1 :: [[Int]]
> add_input1 =
>   [[0,  2,  3],
>    [0,  1,  8],
>    [0, 10,  5],
>    [0, 11,  2],
>    [1,  4,  5]]


The simulation driver for adders

> sim_adder add_circuit n input =
>   let (co,s) = add_circuit ci (bitslice2 x y)
>       ci = getbit   input 0
>       x  = getbin n input 1
>       y  = getbin n input 2
>       simoutput :: [Format Bool]
>       simoutput =
>         [string " ci=", bit ci,
>          string " x= ", bindec 3 x,
>          string " y= ", bindec 3 y,
>          string "   Output: ", bit co, bindec 4 s]
>   in do putStrLn "\nSimulating ripple carry adder"
>         run input simoutput

Simulating the Ripple Carry Adder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> sim_ripadd =
>   let n = 8
>       input =
>        [[0,  3,  8],
>         [0,  5, -9],
>         [0, -2, -3],
>         [0,  4, 50]]
>       ci = getbit   input 0 :: Stream Bool 
>       x  = getbin n input 1
>       y  = getbin n input 2
>       (co,s) = rippleAdd ci (zipn n x y)
>       simoutput :: [Format Bool]
>       simoutput =
>         [bit ci,
>          string " x= ", bindec 4 x, tcdec 4 x,
>          string " y= ", bindec 4 y, tcdec 4 y,
>          string " Output: ", bit co,
>          string " sum= ", bindec 4 s, tcdec 4 s]
>   in run input simoutput

This module contains examples and test cases for the circuits defined
in WordComb.

Comparison
~~~~~~~~~~

> cmp_input1 :: [[Int]]
> cmp_input1 =
>   [[2, 3],
>    [3, 2],
>    [3, 3],
>    [1, 8],
>    [8, 1],
>    [9, 9],
>    [0, 5],
>    [7, 5]]


> sim_comparitor circuit n input =
>   let (lt,eq,gt) = circuit (bitslice2 x y)
>       x = getbin n input 0
>       y = getbin n input 1
>       simoutput :: [Format Bool]
>       simoutput =
>         [string " x= ", bindec 4 x,
>          string "   y= ", bindec 4 y,
>          string "  (lt,eq,gt) = ",
>          bit lt, bit eq, bit gt]
>   in do putStrLn "\nSimulating comparitor"
>         run input simoutput




old versions...

> {- from here to the end...
> import HydraCLI

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
