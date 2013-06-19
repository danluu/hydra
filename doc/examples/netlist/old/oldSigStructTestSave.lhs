___________________________________________________________________
	 The Hydra Digital Circuit Description Language
	       Examples and Test Cases for SigStruct

Copyright (c) 2001 John O'Donnell.  See the README file for general
information and documentation, COPYING for the full copyright, and
the web page for updates: http://www.dcs.gla.ac.uk/~jtod/Hydra/
___________________________________________________________________

> module SigStructTest where

> import Signal
> import SigStruct
> import SigBool
> import SigStream
> import BasicComb
> import BasicSeq
> import HydraCLI
> import HaskTools

How to use this module
~~~~~~~~~~~~~~~~~~~~~~

This module contains a set of examples that illustrate how
structural signals work.  These examples can be used as a tutorial,
and they can also be used to test the software.  Most of the
examples are followed by test cases indicated as follows:

*** {test expression} ==> {expected result}

Enter the expression into an interactive Haskell system, and it
should produce the expected result.  You can also run some of the
tests as a batch by executing main :: IO ().


Global definitions for this module
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Many of the examples in this file use a structural signal type
based on Bool or Stream Bool.

> type BSig    = StrucSig Bool ()
> type BoolStrucSig = StrucSig (Stream Bool) ()


Input Port
~~~~~~~~~~

A structural input signal, with a simulation using its extracted
behavior; for example,

> testinput x = altsem (input "x" x)

*** testinput False ==> False
*** testinput True  ==> True


Comparisons of signals and boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This example test the sigEqInBox and boxEqInBox functions, by
defininf a box using the constructors directly and then testing the
signals and subboxes for equality.

> testEq =
>   let box = Box
>         { boxSpecies  = "explicit_circ"
>         , boxTag      = undefined
>         , boxParent   = undefined
>         , boxChildren = [sigOwner x, sigOwner y]
>         , boxInPorts  = [asgl,bsgl]
>         , boxOutPorts = ysgl
>         , boxSignals  = [ainp,binp,x,y,z]
>         , boxWires    = []
>         }
>       a =  input "alpha" False
>       b =  input "beta" False
>       ainp    = Alias InPort (ParentCluster asgl 0) a
>       asgl    = Singleton (Just "a") (TopCluster box 0) ainp
>       binp    = Alias InPort (ParentCluster bsgl 0) b
>       bsgl    = Singleton (Just "b") (TopCluster box 1) binp
>       x       = and2Struc ainp binp
>       x'      = Alias Local (ParentCluster xsgl 0) x
>       xsgl    = Singleton (Just "x") (TopCluster box 2) x'
>       y       = or2Struc x binp
>       z       = or2Struc x x
>       y'      = Alias OutPort (ParentCluster ysgl 0) y
>       ysgl    = Singleton (Just "y") (TopCluster box 3) y'
>       bx      = sigOwner x
>       by      = sigOwner y
>       bz      = sigOwner z
>       (tr_bs,tr_ss) = traceback [] [y] [] []
>       foo s1 s2 =
>         case cmpSigLoc (sigParent s1) (sigParent s2) of
>           Nothing -> "Nothing"
>           Just (b1,b2) ->
>             let ps = boxInPorts b1
>                 qs = boxInPorts b2
>             in  boxSpecies b1 ++ showboxInputs b1
>                 ++ "//\n" ++ boxSpecies b2 ++ showboxInputs b2
>                 ++ "\n"
>   in do putStrLn "temp experimetn"
>         putStrLn ("a=a " ++  show (sigEqInBox ainp ainp))
>         putStrLn ("a=b " ++  show (sigEqInBox ainp binp))
>         putStrLn ("a=x " ++  show (sigEqInBox ainp x))
>         putStrLn ("a=y " ++  show (sigEqInBox ainp y))
>         putStrLn ("a=z " ++  show (sigEqInBox ainp z))
>         putStrLn ("b=b " ++  show (sigEqInBox binp binp))
>         putStrLn ("b=x " ++  show (sigEqInBox binp x))
>         putStrLn ("b=y " ++  show (sigEqInBox binp y))
>         putStrLn ("b=z " ++  show (sigEqInBox binp z))
>         putStrLn ("x=x " ++  show (sigEqInBox x x))
>         putStrLn ("x=y " ++  show (sigEqInBox x y))
>         putStrLn ("x=z " ++  show (sigEqInBox x z))
>         putStrLn ("y=y " ++  show (sigEqInBox y y))
>         putStrLn ("y=z " ++  show (sigEqInBox y z))
>         putStrLn ("z=z " ++  show (sigEqInBox z z))
>         putStrLn ("bx: " ++ boxSpecies bx)
>         putStrLn ("by: " ++ boxSpecies by)
>         putStrLn ("bz: " ++ boxSpecies bz)
>         putStrLn ("bx=bx " ++ show (boxEqInBox bx bx))
>         putStrLn ("bx=by " ++ show (boxEqInBox bx by))
>         putStrLn ("bx=bz " ++ show (boxEqInBox bx bz))
>         putStrLn ("by=bx " ++ show (boxEqInBox by bx))
>         putStrLn ("by=by " ++ show (boxEqInBox by by))
>         putStrLn ("by=bz " ++ show (boxEqInBox by bz))
>         putStrLn ("bz=bx " ++ show (boxEqInBox bz bx))
>         putStrLn ("bz=by " ++ show (boxEqInBox bz by))
>         putStrLn ("bz=bz " ++ show (boxEqInBox bz bz))
>         putStrLn ("#bs=" ++ show (length tr_bs))
>         putStrLn ("#ss=" ++ show (length tr_ss))
>         putStrLn ("a/b " ++ foo a b)
>         putStrLn ("b/x " ++ foo b x)
>         putStrLn ("x/y " ++ foo x y)
>         putStrLn ("y/z " ++ foo y z)


Explicit Construction of Primitive Black Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> explicit_and2
>   :: Signal a
>   => String -> String -> String
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> explicit_and2 labin1 labin2 labout a b =
>   let box =
>         Box { boxSpecies  = "explicit_and2"
>             , boxTag      = undefined
>             , boxParent   = undefined
>             , boxChildren = []
>             , boxInPorts  = [asgl,bsgl]
>             , boxOutPorts = xsgl
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ainp    = Alias InPort (ParentCluster asgl 0) a
>       asgl    = Singleton (Just labin1) (TopCluster box 0) ainp
>       binp    = Alias InPort (ParentCluster bsgl 0) b
>       bsgl    = Singleton (Just labin2) (TopCluster box 1) binp
>       xsem    = and2 (altsem a) (altsem b)
>       x       = Definer (ParentCluster xsgl 0) xsem
>       xout    = Alias OutPort (ParentCluster xsgl 0) x
>       xsgl    = Singleton (Just labout) (TopCluster box 2) xout
>   in xout

> explicit_or2
>   :: Signal a
>   => String -> String -> String
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> explicit_or2 labin1 labin2 labout a b =
>   let box =
>         Box { boxSpecies  = "explicit_or2"
>             , boxTag      = undefined
>             , boxParent   = undefined
>             , boxChildren = []
>             , boxInPorts  = [asgl,bsgl]
>             , boxOutPorts = xsgl
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ainp    = Alias InPort (ParentCluster asgl 0) a
>       asgl    = Singleton (Just labin1) (TopCluster box 0) ainp
>       binp    = Alias InPort (ParentCluster bsgl 0) b
>       bsgl    = Singleton (Just labin2) (TopCluster box 1) binp
>       xsem    = or2 (altsem a) (altsem b)
>       x       = Definer (ParentCluster xsgl 0) xsem
>       xout    = Alias OutPort (ParentCluster xsgl 0) x
>       xsgl    = Singleton (Just labout) (TopCluster box 2) xout
>   in xout

The following circuit, whose black box representation is
constructed explicitly, corresponds to

     circ1 a b = or2 (and2 a b) b

> circ1
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b

> circ1 a b =
>   let box =
>         Box { boxSpecies  = "circ1"
>             , boxTag      = undefined
>             , boxParent   = undefined
>             , boxChildren = [sigOwner x, sigOwner y]
>             , boxInPorts  = [asgl,bsgl]
>             , boxOutPorts = ysgl
>             , boxSignals  = [ainp,binp,x,y]
>             , boxWires    = []
>             }
>       ainp    = Alias InPort (ParentCluster asgl 0) a
>       asgl    = Singleton (Just "circ1_a_inp")
>                   (TopCluster box 0) ainp
>       binp    = Alias InPort (ParentCluster bsgl 0) b
>       bsgl    = Singleton (Just "circ1_b_inp")
>                   (TopCluster box 1) binp
>       x       = and2Struc ainp binp
>       xclu    = sigCluster x
>       y       = or2Struc x binp
>       yclu    = sigCluster y
>       yout    = Alias OutPort (ParentCluster ysgl 0) y
>       ysgl    = Singleton (Just "circ1_y_out")
>                   (TopCluster box 3) yout
>   in yout

> circ2
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b

> circ2 =
>   let f a b = or2Struc b (and2Struc a b)
>   in box21Struc
>         "circ2" "circ2_a_inp" "circ2_b_inp" "circ2_x_out"
>         undefined f

> circ3
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b

> circ3 =
>   let f a b =
>         explicit_and2 "A1/a" "A1/b" "A1/x"
>           (explicit_or2 "O2/a" "O2/b" "O2/x"
>               (explicit_and2 "A4/a" "A4/b" "A4/x" a b)
>               (explicit_and2 "A5/a" "A5/b" "A5/x" b a))
>           (explicit_or2 "O3/a" "O3/b" "O3/x"
>               (explicit_and2 "A6/a" "A6/b" "A6/x" a a)
>               (explicit_and2 "A7/a" "A7/b" "A7/x" b b))
>   in  box21Struc "circ3" "circ3_a_inp" "circ3_b_inp"
>          "circ3_x_out" undefined f



> test_circ3 =
>   showHandle (circ3 (input "alpha" False) (input "beta" False))

> show_circ3_directly =
>   let handle = circ3 (input "alpha" False) (input "beta" False)
>       box    = sigOwner handle
>   in do putStrLn "show_circ3_directly"
>         showboxfull 0 box


Test cases for running the circuit definitions given above:


> reg1struc :: BoolStrucSig -> BoolStrucSig -> BoolStrucSig
> reg1struc =
>   let f ld a =
>         let x = tag 100 (dff (mux1Struc ld x a))
>         in x
>   in box21Struc "reg1" "reg1_ld" "reg1_a" "reg1_x" undefined f


Tools for showing circuit structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The print_signal function describes a structural signal by giving
its label, if any, and the type of circuit that produced it.

> print_signal :: Signal a => StrucSig a b -> IO ()
> print_signal x =
>   do putStrLn ("Signal " ++ describe_sig x)
>      putStrLn "produced by "
>      putStrLn (boxSpecies (sigOwner x))

> print_handle :: Signal a => StrucSig a b -> IO ()
> print_handle = print_box . sigOwner

This function shows the contents of a black box circuit.

> print_box :: Signal a => Box a b -> IO ()
> print_box box =
>   do putStrLn ("Black box " ++ boxSpecies box)
>      let inps = boxInPorts box
>      putStrLn (show (length inps) ++ " Inports")
>      doall (\(i,c) -> putStrLn (showcluster i c)) (zip [0..] inps)
>      putStrLn (showcluster 0 (boxOutPorts box))
>      let cs = boxChildren box
>      putStrLn (show (length cs) ++ " subboxes")
>      doall (\(i,p) -> putStrLn (showboxn i p)) (zip [0..] cs)
>      return ()

> showboxn :: Signal a => Int -> Box a b -> String
> showboxn i b = "subbox " ++ show i ++ ": " ++ boxSpecies b


Defining a primitive logic gate
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Primitive structural gates are defined using functions like
prim21Struc, as follows:

> my_and2 :: StrucSig Bool d -> StrucSig Bool d -> StrucSig Bool d
> my_and2 = prim21Struc (&&) ["a","b"] "x" "and2" undefined

They can be simulated by giving them a structural input, and using
altsem to extract the result from the structural output.

> ytest :: Bool -> Bool -> Bool
> ytest a b =
>   altsem (my_and2 (input "alpha" a) (input "beta" b))

*** ytest False True ==> False
*** ytest True True  ==> True


Structural instances of logic gates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The SigStruct module defines instances of the logic gates with
structural types, and these can be used directly (but only on
structural types).

 and2Struc, or2Struc, xor2Struc
   :: Signal a
   => StrucSig a b -> StrucSig a b -> StrucSig a b

> ztest :: Bool -> Bool -> Bool
> ztest a b =
>   altsem (and2Struc (input "alpha" a) (input "beta" b))

*** ztest False True ==> False
*** ztest True True  ==> True


Standard overloaded logic gates
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A structural inverter with its simulation; run it by evaluating
invstrucbeh True

> invstrucB :: StrucSig Bool () -> StrucSig Bool ()
> invstrucB = inv
> invstrucbeh x = altsem (invstrucB (input "x" x))

Now we test the behaviour of a structural and2 gate.

> and2strucB
>   :: StrucSig Bool () -> StrucSig Bool () -> StrucSig Bool ()
> and2strucB = and2

> and2strucbeh a b =
>   altsem (and2strucB (input "a" a) (input "b" b))


Defining a black box circuit
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ff circuit has a complete black box structure, with labeled
components and signals.  Its internal connectivity ff' is defined
by a local equation, which has exactly the same form as an ordinary
non-structural circuit definition.

> {-
> ff :: Signal a => a -> a -> a
> ff =
>   box21 "example ff" "a""b" "c" ff'
>     where ff' a b = and2 (inv a) b
> -}

The ff circuit is overloaded, and uses the Signal class, so it can
be simulated by applying it to pure behavioral signals.

*** ff False True ==> True
*** ff True True ==> False
*** ff (zero :: Stream Bool) (one :: Stream Bool) ==> 1,1,1,...
*** ff (one  :: Stream Bool) (one :: Stream Bool) ==> 0,0,0,...

We can also construct a structural ff circuit, simply by using
structural input signals.  In this case altsem is used to extract
the simulation meaning of the circuit's output.

> {-
> ffcirc
>   :: StrucSig Bool () -> StrucSig Bool () -> StrucSig Bool ()
> ffcirc = ff

It can be simulated by extracting its behaviour from the structure:

> ffcircbeh a b = altsem (ffcirc (input "a" a) (input "b" b))
> -}


Showing the structure of a black box
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> {- run_and2 :: IO ()
> run_and2 =
>   do let p = input "p" (zero :: Bool)
>      let q = input "q" one
>      let r = and2 p q
>      run r --showBox 0 (sigOwner r) -}


Constructing a black box directly (example: multiplexor)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To illustrate how black box circuits are defined, we begin with a
direct definition of mux1 as a black box structure.  The data
structure is defined explicitly, which makes the definition
straightforward but rather long and detailed.

> {-
> mux1_a
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b

> mux1_a ctl_inp a_inp b_inp =
>   let ctl_loc = Alias box (ParentCluster ctl_clu 0) ctl_inp
>       ctl_clu = Singleton (Just "ctl") (InPortClu box 0) ctl_loc
>       a_loc = Alias box (ParentCluster a_clu 0) a_inp
>       a_clu = Singleton (Just "a") TopCluster a_loc
>       b_loc = Alias box (ParentCluster b_clu 0) b_inp
>       b_clu = Singleton (Just "b") TopCluster b_loc
>       x1 = inv ctl_loc
>       x1_clu = Singleton (Just "x1") TopCluster x1
>       x2 = and2 x1 a_loc
>       x2_clu = Singleton (Just "x2") TopCluster x2
>       x3 = and2 ctl_loc b_loc
>       x3_clu = Singleton (Just "x3") TopCluster x3
>       x4 = or2 x2 x3
>       x4_clu = Singleton (Just "x4") TopCluster x4
>       x4' = Alias box (ParentCluster x4_clu 0) x4
>       box = Box
>        {boxSpecies = "mux1_a",
>         boxTag = undefined,
>         boxParent = undefined, -- sigParent ctl_inp,
>         boxChildren = [sigOwner x1, sigOwner x2, sigOwner x3],
>         boxInPorts = [ctl_clu,a_clu,b_clu],
>         boxInternal = [x1_clu,x2_clu,x3_clu],
>         boxOutPorts = x4_clu,
>         boxWires =
>           [(ctl_loc,[(Internal,[0,0])])
>           ]}
>   in x4'
> -}

Here is an IO operation that traverses and shows the data structure
all in one one batch.  Normally we would use either a command line
interface or a graphical user interface to allow the user to
control such traversals interactively.

> {-
> run_mux1a :: IO ()
> run_mux1a =
>  do let c = input "control" (zero :: Bool)
>     let a = input "inputA" zero
>     let b = input "inputB" one
>     let out = mux1_a c a b
>     let box = sigOwner out
>     showBox 0 box

> showBox :: Signal a => Int -> Box a d -> IO ()
> showBox depth box =
>   do let cs = boxChildren box
>      let indent = take (2*depth) (repeat ' ')
>      putStr (indent ++ if length cs == 0
>              then "Primitive component "
>              else "Black box ")
>      putStr (indent ++ boxSpecies box ++ "\n")
>      let inports = boxInPorts box
>      putStr (indent ++ "  InPorts    = "
>              ++ showClusters inports ++ "\n")
>      let internals = boxInternal box
>      putStr (indent ++ "  Internals  = "
>              ++ showClusters internals ++ "\n")
>      let outports = boxOutPorts box
>      putStr (indent ++ "  OutPorts   = "
>              ++ showCluster outports ++ "\n")
>      if length cs == 0
>        then return ()
>        else putStr (indent ++ "  Subsystems = "
>                     ++ identifyBoxes cs ++ "\n")
>      showSubBoxes (depth+1) 0 cs


> showSubBoxes :: Signal a => Int -> Int -> [Box a d] -> IO ()
> showSubBoxes depth i [] = return ()
> showSubBoxes depth i (b:bs) =
>   do let indent = take (2*depth) (repeat ' ')
>      putStr (indent ++ "Subsystem " ++ show i ++ ":\n")
>      showBox depth b
>      showSubBoxes depth (i+1) bs
> -}



Here is an old one, using the box function.

> {-
> mux1_1
>   :: Signal a
>   => StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a
> mux1_1 c_ x_ y_ =
>   let ([c,x,y], [z_]) =
>         box "mux1_1" ""
>           [(c_,"mux_c_inp"),(x_,"x_inp"),(y_,"y_inp")]
>           [(z,"mux_z_output")]
>       z = or2 (and2 (inv c) x) (and2 c y)
>    in z_
> -}

> boxedInv :: Signal a => StrucSig a b -> StrucSig a b
> boxedInv =
>   box11Struc "boxedInv" "boxinv-a" "boxinv-x" undefined inv

> circ4 :: Signal a => StrucSig a b -> StrucSig a b
> circ4 =
>  box11Struc "circ4" "foo" "bar" undefined boxedInv

HERE ARE THE CIRCUITS BEING TESTED

> mux1Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b
> mux1Struc =
>   let f ctl a b = or2 (and2 (inv ctl) a) (and2 ctl b)
>   in box31Struc "mux1Struc" "ctl" "a" "b" "x" undefined f

> mux2Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> StrucSig a b

> mux2Struc =
>   let f ctl0 ctl1 a b c d =
>         mux1Struc ctl0
>           (mux1Struc ctl1 a b)
>           (mux1Struc ctl1 c d)
>  in box61Struc "mux2Struc" "ctl0" "ctl1"
>       "mux2_a" "mux2_b" "mux2_c" "mux2_d"
>       "mux2_x"
>       undefined f


> halfAddStruc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b
>   -> (StrucSig a b, StrucSig a b)

> halfAddStruc =
>   let f x y = (and2 x y, xor2 x y)
>   in box22Struc "halfAdd" "ha_x" "ha_y" "ha_cout" "ha_sum"
>        undefined f

> twotwo
>   :: Signal a
>   => StrucSig a b -> StrucSig a b
>   -> (StrucSig a b, StrucSig a b)
> twotwo =
>   let f x y =
>         let (p,q) = halfAddStruc (invStruc x) y
>         in (p, q)
>   in box22Struc "twotwo" "22_a""22_b" "22_x""22_y" undefined f


> fullAddStruc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> (StrucSig a b, StrucSig a b)

> fullAddStruc =
>   let f cin x y =
>         let (c1,s1)  = halfAddStruc x y
>             (c2,sum) = halfAddStruc s1 cin
>         in (xor2 c1 c2, sum)
>   in box32Struc "fullAdd" "fa_cin" "fa_x" "fa_y"
>        "fa-cout" "fa-sum" undefined f





Tool support for constructing a black box
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In realistic circuit specification, we want to reduce the syntactic
burden of definitions like the previous one.  The solution is to
use a helper function to put the data structure together.  The
helper function is specialized for the number of inputs and
outputs; therefore the mux1 circuit is defined using the box31Struc
function.

> {-
> mux1_2 :: Signal a => StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a
> mux1_2 = box31Struc "mux1_2" ["a","b","c"] ["x"] f
>   where f a b c = or2 (and2 (inv a) b) (and2 a c)
> -}


Overloaded construction of black boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In normal practice, it would be most convenient to begin by
defining a circuit rather abstractly, showing how it is defined in
terms of lower level circuits.  Such a specification is behavioral
because it doesn't allow the structures of circuits with feedback
to be determined.  Here is an example (without feedback):

> {-
> mux1_connect :: Signal a => a -> a -> a -> a
> mux1_connect c x y = or2 (and2 (inv c) x) (and2 c y)
> -}

Now the black box structural specification is easily defined using
the box31Struc function (or similar ones, depending on the circuit
type).  The nonstructural specification is reused; one needs only
to give the names of the circuit and ports.  Naturally, this style
of specification doesn't allow individual names to be given to
internal signals, but that isn't needed anyway for small circuits.

> {-
> mux1_3 :: Signal a => StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a
> mux1_3 = box31Struc "mux1_3" ["a","b","c"] ["x"] mux1_connect
> -}

A problem now is that mux1_3 cannot be used with a non-structural
signal type like Bool.  Yet it would be quite reasonable to type in
"mux1 True False True" and expect to receive the result "True".
What we really want is the ability to use arbitrary signal types
with all the circuits.  If the circuit is defined without
structure, like mux1_connect above, then if it's executed with a
structural signal type it will be a "white box" structure (i.e. the
only structure will come from whatever circuit or test harness it's
embedded in).  If the circuit is defined with box31Struc (or
similar function) we would like it still to work with a pure Bool
signal.  Another job for overloading!

Here is the idea.  In the first place, the circuit should have just
a plain type like mux1 :: Signal a => a -> a -> a -> a, even if
it's defined using box31Struc.  This means that box31Struc itself
must be overloaded; if the signal type is in a Structural class,
then it builds its graph and produces a StrucSig result.  Otherwise
the port names and other structural information are ignored, and it
simply produces the simple behavioral type.

mux1_4 :: Signal a => a -> a -> a -> a
mux1_4 = box31Struc' "mux1_3" ["a","b","c"] ["x"] f
  where f c x y = or2 (and2 (inv c) x) (and2 c y)

Here is how I want to define it, allowing it to be defined as a
black box with structure, yet also allowing it to run just as a
pure behavioral version (for Bool or Stream Bool signal type, for
example).

> {-
> mux1_4 :: Signal a => a -> a -> a -> a
> mux1_4 = box31 "mux1_4"  ["a","b","c"] ["x"] f
>   where f c x y = or2 (and2 (inv c) x) (and2 c y)
> -}

Here is an older definition using bb, along with its test case:

> {-
> mux1 c_ x_ y_ =
>   let ([c,x,y], [z_]) =
>         bb "mux1" [(c_,"c"),(x_,"x"),(y_,"y")] [(z,"z")]
>       z = or2 (and2 (inv c) x) (and2 c y)
>    in z_
>
> test_mux1 =
>   do putStr "\nThis is test_mux1\n"
>      let c = circuitInput "ctrl"
>      let a = circuitInput "alpha"
>      let b = circuitInput "beta"
>      let out = mux1 c a b
>      let thisbox = sigBox out
>      let inps = boxInputs thisbox
>      putStr ("  this box name: " ++ boxName thisbox ++ "\n")
>      putStr ("  local output name: " ++ sigName out ++ "\n")
>      putStr ("  local input names: "
>                ++ concat (map ((' ':) . sigName) inps) ++ "\n")
>      let bs = traceback (intgens [out]) []
>      putStr ("  " ++ show (length bs) ++ " boxes inside: "
>                ++ concat (map boxName bs) ++ "\n")
>      return ()
> -}


Half Adder
~~~~~~~~~~

> {-
> hatest :: StrucSig Bool d -> StrucSig Bool d -> (Bool,Bool)
> hatest x y =
>   let(c,s) = halfAdd x y
>   in (altsem c, altsem s)
> -}

> {-
> halfAdd1
>   :: Signal a 
>   => StrucSig a d -> StrucSig a d
>   -> (StrucSig a d, StrucSig a d)

> halfAdd1 x y =
>   let box = Box {boxSpecies = "halfAdd",
>                  boxTag = undefined,
>                  boxParent = ChildOfBox (sigOwner x),
>                  boxChildren = [andgate,orgate],
>                  boxInPorts = [x'',y''],
>                  boxInternal = [],
>                  boxOutPorts = out}
>       c = and2 x y
>       s = xor2 x y
>       x' = Alias (ParentCluster x'' 0) x
>       x'' = Singleton (Just "x") (ParentBox box InPort 0) x'
>       y' = Alias (ParentCluster y'' 0) y
>       y'' = Singleton (Just "y") (ParentBox box InPort 1) y'
>       c' = Alias (ParentCluster c'' 0) c
>       c'' = Singleton (Just "c") (ParentCluster out 0) c'
>       s' = Alias (ParentCluster s'' 0) s
>       s'' = Singleton (Just "s") (ParentCluster out 1) s'
>       out = Tuple2 (Just "halfAdd-cs") (ParentBox box Internal 0)
>               c'' s''
>       andgate = sigOwner c
>       orgate = sigOwner s
>   in (c',s')

> halfAdd2
>   :: Signal a 
>   => StrucSig a d -> StrucSig a d
>   -> (StrucSig a d, StrucSig a d)

> halfAdd2 =
>   box22Struc "halfAdd" "x" "y" "sum2" ("c","s") f
>   where f x y = (and2 x y, xor2 x y)


> hatest1 :: StrucSig Bool d -> StrucSig Bool d -> (Bool,Bool)
> hatest1 x y =
>   let(c,s) = halfAdd1 x y
>   in (altsem c, altsem s)

> hatest2 :: StrucSig Bool d -> StrucSig Bool d -> (Bool,Bool)
> hatest2 x y =
>   let(c,s) = halfAdd2 x y
>   in (altsem c, altsem s)
> -}


Full adder using structural signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> {-
> fullAddStruct
>   :: Signal a
>   => StrucSig a d -> (StrucSig a d, StrucSig a d)
>   -> (StrucSig a d, StrucSig a d)

> fullAddStruct a (x,y) =
>   let c = or3 (and2 a x) (and2 a y) (and2 x y)
>       s = xor2 a (xor2 x y)
>       out = mkOutStruct "fullAdd"
>               [S "cin" a, T2 "xy" (S "x" x) (S "y" y)]
>               (T2 "sum2" (S "cout" c) (S "s" s))
>       Tuple2 _ _ (Singleton _ _ c') (Singleton _ _ s') = out
>   in (c',s')

> testFullAddStruct
>   :: StrucSig Bool a -> (StrucSig Bool a, StrucSig Bool a)
>   -> (Bool,Bool)

> testFullAddStruct a (x,y) =
>   let (c,s) = fullAddStruct a (x,y)
>   in (altsem c, altsem s)

> runFAstruct :: IO ()
> runFAstruct =
>   let f :: StrucSig Bool d -> StrucSig Bool d -> StrucSig Bool d
>         -> IO ()
>       f a x y =
>         do let (c,s) = fullAddStruct a (x,y)
>            putStr ("  " ++ showsig (altsem a)
>                    ++ showsig (altsem x)
>                    ++ showsig (altsem y) ++ " "
>                    ++ showsig (altsem c) ++ showsig (altsem s)
>                    ++ "\n")
>            return ()
>   in do putStr "(c,s) = fullAdd a (x,y)\n"
>         putStr "  axy cs\n"
>         putStr "  ------\n"
>         f zero zero zero
>         f zero zero one
>         f zero one  zero
>         f zero one  one
>         f one  zero zero
>         f one  zero one
>         f one  one  zero
>         f one  one  one
>         return ()
> -}


Running a batch of tests
~~~~~~~~~~~~~~~~~~~~~~~~

This module defines a lot of examples and test cases as IO
operations.  The main operation collects them together, so they can
be run easily as a batch.

> getinps :: [Int] -> Stream Bool
> getinps [] = zero
> getinps (x:xs) = Cycle (x==1) (getinps xs)

> showOuts :: Stream Bool -> [Int]
> showOuts (Cycle False xs) = 0 : showOuts xs
> showOuts (Cycle True xs)  = 1 : showOuts xs

> main :: IO ()
> main =
>   do let a = input "input_a" False
>      let b = input "input_b" False
>      let c = input "input_c" False
>      let d = input "input_d" False
>      let e = input "input_e" False
>      let f = input "input_f" False
>
>      separatorLine
>      putStrLn "Examples for SigStruct"
>      separatorLine
>      putStrLn "circ1\n"
>      print_handle (circ1 a b)
>      separatorLine
>      putStrLn "circ2\n"
>      print_handle (circ2 a b)
>      separatorLine
>      putStrLn "circ3\n"
>      print_handle (circ3 a b)
>      separatorLine
>      show_circ3_directly
>      separatorLine
>      test_circ3
>      separatorLine
>      showHandle (mux1Struc a b c)
>      separatorLine
>      showHandle (circ4 a)
>      separatorLine
>      putStrLn "mux2 handle\n"
>      showboxfull 0 (sigOwner (mux2Struc a b c d e f))
>
>      separatorLine
>      do putStrLn "doing fulladd"
>         let (cout,sum) = fullAddStruc a b c

>         putStrLn ("\ncout = " ++ describe_sig cout)
>         let cout' = outPortSource cout
>         putStrLn ("cout source is " ++ describe_sig cout')
>         let b1 = sigOwner cout'
>         putStrLn ("cout' owner is " ++ boxSpecies b1)
>
>         putStrLn ("\nsum  = " ++ describe_sig sum)
>         let sum' = outPortSource sum
>         putStrLn ("sum source is " ++ describe_sig sum')
>         let b2 = sigOwner sum'
>         putStrLn ("sum' owner is " ++ boxSpecies b2)
>
>         let box = sigOwner cout
>         putStrLn ("\n top box = " ++ boxSpecies box)
>         showboxquick 0 box
>         let inclus = boxInPorts box
>         putStrLn (" in clusters:\n " ++
>            concat (map ((++"\n").(showcluster 0)) inclus))
>
>         putStrLn "\nb1, the xor gate"
>         showboxquick 0 b1
>         let [xorinp1,xorinp2] = boxinputs b1
>         putStrLn ("xorin1 " ++ describe_sig xorinp1)
>         putStrLn ("xorin2 " ++ describe_sig xorinp2)
>
>         putStrLn "\nb2, the second half adder"
>         let b2inps = boxinputs b2
>         putStrLn ("b2inputs " ++ concat (map describe_sig b2inps))
>         showboxquick 0 b2
>
>         let b3 = sigOwner xorinp1
>         putStrLn "b3, the first half adder"
>         showboxquick 0 b3
>
>         let b3inps = boxinputs b3
>         putStrLn ("b3inputs " ++ concat (map describe_sig b3inps))
>
>         putStrLn ("b1=b1 " ++ show (boxEqInBox b1 b1))
>         putStrLn ("b1=b2 " ++ show (boxEqInBox b1 b2))
>         putStrLn ("b1=b3 " ++ show (boxEqInBox b1 b3))
>
>         putStrLn ("b2=b1 " ++ show (boxEqInBox b2 b1))
>         putStrLn ("b2=b2 " ++ show (boxEqInBox b2 b2))
>         putStrLn ("b2=b3 " ++ show (boxEqInBox b2 b3))
>
>         putStrLn ("b3=b1 " ++ show (boxEqInBox b3 b1))
>         putStrLn ("b3=b2 " ++ show (boxEqInBox b3 b2))
>         putStrLn ("b3=b3 " ++ show (boxEqInBox b3 b3))
>
>         putStrLn ("xori1=xori1 " ++
>               show (sigEqInBox xorinp1 xorinp1))
>         putStrLn ("xori2=xori2 " ++
>               show (sigEqInBox xorinp2 xorinp2))
>         putStrLn ("xori1=xori2 " ++
>               show (sigEqInBox xorinp1 xorinp2))
>
>         let tr = traceback [] [sum',cout'] [] []
>         let trbs = fst tr
>         let trss = snd tr
>         putStrLn (show (length trbs) ++ "..."
>                     ++ show (length trss))
>         putStrLn "first subcirc"
>         showboxquick 0 (head trbs)
>         putStrLn "first sig"
>         putStrLn (describe_sig (head trss))
>         putStrLn ("subcircuits " ++
>             concat (map boxSpecies (take 100  trbs)))
>         putStrLn "\n\nThe Full Box\n"
>         showboxfull 0 box
>         return ()
>
>
>      separatorLine
>      putStrLn "register 1 circuit structure!"
>      let ldinp = [0,0,1,0,0,0,1,0,0,0,1,1,1]
>      let ainp  = [0,0,1,0,0,0,0,1,1,0,1,0,0]
>      let aa = input "input_ld" (getinps ldinp)
>      let bb = input "input_aa" (getinps ainp)
>      let y = reg1struc aa bb
>      showboxfull 0 (sigOwner y)
>      separatorLine
>      putStrLn "register 1 circuit simulation!"
>      let yval = altsem y
>      putStrLn (show (take (length ldinp) (showOuts yval)))
>
>      separatorLine
>      putStrLn "SigStructTest finished"
>      return ()


> go =
>   do let a = input "input_a" False
>      let b = input "input_b" False
>      let c = input "input_c" False
>      let d = input "input_d" False
>      let e = input "input_e" False
>      let f = input "input_f" False
>      separatorLine
>      putStrLn "TwoTwo circuit"
>      let (u,v) = twotwo a b
>
>      putStrLn ("\n Outputs\nOut u "++ describe_sig u)
>      let u' = outPortSource u
>      putStrLn ("Out u' "++ describe_sig u')
>      putStrLn ("Out v "++ describe_sig v)
>      let v' = outPortSource v
>      putStrLn ("Out v' "++ describe_sig v')
>
>      putStrLn "\nmain box b1"
>      showboxquick 0 (sigOwner u)

>      putStrLn "\nbox b2"
>      let b2 = sigOwner u'
>      showboxquick 0 b2
>
>      putStrLn "Inputs to b2 half adder"
>      let [b2i0,b2i1] = boxinputs b2
>      putStrLn ("b2 input 0 = " ++ describe_sig b2i0)
>      putStrLn ("b2 input 1 = " ++ describe_sig b2i1)

>      putStrLn "\nInv box b3"
>      let b3 = sigOwner b2i0
>      showboxquick 0 b3

>      putStrLn "Inputs to b3 inverter"
>      let [b3i0] = boxinputs b3
>      putStrLn ("b3 input 0 = " ++ describe_sig b3i0)
>
>      let sss =  [b3i0,b2i1,u',v',b2i0]  -- b2i0 bad
>      let tmp = map (sigEqInBox u') sss
>      putStrLn (show tmp)
>
>      let (trbs,trss) = traceback [] [b3i0,b2i0,b2i1] [] []
>      putStrLn ("#bs " ++ show (length trbs))
>      putStrLn ("#ss " ++ show (length trss))
>      let u'parent = sigParent u'
>      let b2i0parent = sigParent b2i0
>      let foo = cmpSigLoc u'parent b2i0parent
>      putStrLn "cmpSigLoc u'b2i0"
>      case foo of
>        Nothing -> putStrLn "got Nothing"
>        Just (zbox1,zbox2) -> putStrLn "got two boxes"
>
>      showHandle u
>      putStrLn "The Full Box!"
>      showboxfull 0 (sigOwner u)
>      separatorLine
