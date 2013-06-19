---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module BitComb4Test where

This module contains examples and test cases for the circuits defined
in BitComb4.

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




