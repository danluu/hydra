======================================================================
exp1.hs

data Foo = forall a. MkFoo a (a->Bool)
         | Nil

-- xs :: [Foo]
xs = [MkFoo 3 even, MkFoo 'c' isUpper]

======================================================================
exp.lhs

Signal Clusters
~~~~~~~~~~~~~~~

This is the approach I tried Saturday, Nov 11, but it seems to run
into type ambiguity problems (infinite unification).  Therefore I
will try a different approach...


 data ClusterTree a
   = Singleton a
   | Tuple2 (ClusterTree a) (ClusterTree a)
   | Tuple3 (ClusterTree a) (ClusterTree a) (ClusterTree a)
   | Word [ClusterTree a]

 class Cluster a where
   toCluster :: a -> ClusterTree a
   fromCluster :: ClusterTree a -> a

 instance Cluster Bool where
   toCluster x = Singleton x
   fromCluster (Singleton x) = x

 instance (Cluster a, Cluster b) => Cluster (a,b) where
 --   toCluster (a,b) = Tuple2 a b
   fromCluster (Tuple2 a b) = (a,b)

 frCl :: Cluster x -> x
 fromCluster (Singleton x) = x
 fromCluster (Tuple2 x y) = (fromCluster x, fromCluster y)


instance Cluster Bool where
  toCluster = Singleton


======================================================================
codefragments
this should all be in SigStructTestSave.lhs, this is just for safety
> import BasicComb
> import BasicSeq

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


======================================================================
TypeExp.lhs

a = base type of cluster
b = base type of representation
c = type

> class Clusterable a b c where
>   getsigs :: (a->b) -> (c -> [b])

> instance Clusterable Bool b c where
>   getsigs f x = [f x]

> {-instance
>   (Clusterable a c, Clusterable b c)
>   => Clusterable (a,b) c
>   where
>     getsigs (x,y) = getsigs x ++ getsigs y -}

> t1 = getsigs True
> {- t2 = getsigs (False,True)
> t3 = getsigs (False,(True,False))
> t4 = getsigs ((False,True),(True,False)) -}

> main =
>   do putStr "Hello"
>      return ()




======================================================================
SigStructExper.lhs
--------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
		Structural Representation of Circuits

Copyright (c) 2001 John O'Donnell.  See the README file for documentation,
COPYING for the full copyright, and the web page for further infomation
and the latest version: http://www.dcs.gla.ac.uk/~jtod/Hydra/
--------------------------------------------------------------------------

> module SigStruct where

SigStruct defines the structural representation of signals and black
box circuits.  These can be used to provide a variety of services that
require knowledge of the structure of a circuit as well as its
semantics, including the generation of netlists and the traversal
circuits in order to find signals or components.

> import Signal
> import HaskTools
> import SigBool  -- for testing, just for the time being


Representation of Black Box Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Box type describes the structure of a black box circuit.  It uses the
following polymorphic types:

  a = behavioral semantics type  (e.g. Stream Bool)
  b = layout representation type (e.g. geometric positions)

> data Signal a => Box a b
>   = Box
>      { boxSpecies  :: String
>      , boxLayout   :: b
>      , boxParent   :: BoxParent a b
>      , boxChildren :: [Box a b]
>      , boxInPorts  :: [ClusterTree (StrucSig a b) (Box a b)]
>      , boxOutPorts :: ClusterTree (StrucSig a b) (Box a b)
>      , boxSignals  :: [StrucSig a b]
>      , boxWires    :: [Wire (StrucSig a b)]
>      }

The boxSpecies is a generic circuit name, indicating what kind of circuit
it is, such as "half adder" or "74x000".  The boxLayout can be used to
attach any additional information to the box, such as geometric
positioning of the components and wires.

The boxParent specifies where a circuit lies within the entire circuit
hierarchy.  It is either a top box (which means the box is not a child of
any known box; there may be several top boxes), or else it is a child
within a parent box, with an integer index giving its position within the
boxChildren list of the parent.  parent's list of children.

> data BoxParent a b
>   = TopBox
>   | ChildBox (Box a b) Int

The boxChildren field contains a list of the black boxes used
within the box.  Only circuits mentioned explicitly in the box
definition are listed; circuits that are more deeply nested do not
appear in boxChildren, but they can be obtained by a recursive
traversal.


Structural Representation of Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A structural signal describes exactly where in the circuit the
signal is found.  Its type parameters are

  a = base signal behavioral semantics (e.g. Stream Bool)
  b = box layout type

The signal location is specified by a reference to a ClusterTree.

> data StrucSig a b
>   = Definer a (ClusterTree (StrucSig a b) (Box a b))
>   | Alias (StrucSig a b) Port
>       (ClusterTree (StrucSig a b) (Box a b))

Aliased signals are marked according to their status within a box,
using the Port type.  A Tag is used to mark a point on feedback
loops, to prevent structure traversals from falling into infinite
recursions.

> data Port =
>   InPort | OutPort | BiPort | Local | BiLocal | Tag Int
>   deriving Show

A structural signal contains an alternative semantics (for example,
a stream).  The altsem function returns the alternative semantics
associated with a structural signal.

> behavior :: StrucSig a b -> a
> behavior (Definer x _) = x
> behavior (Alias x _ _) = behavior x


Traversing a circuit graph
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are three groups of signal owned by a box: its input ports,
its output ports, and signals that are internal to the box (these
must be outputs produced by children of the box).  These are all
available in the boxPorts field.

> child :: Signal a => Box a b -> Int -> Box a b
> child b i = boxChildren b !! i

Every signal belongs to exactly one black box which is called the
`owner' of the signal.  From the viewpoint of a black box, every
signal that belongs to it is either an Input, and Output, or a
Internal signal.

> sigOwner :: Signal a => StrucSig a b -> Box a b
> sigOwner = clusterTreeOwner . sigClusterTree

Each structural signal exists within a cluster, which has a
corresponding cluster tree.  The following function obtains that
tree.

> sigClusterTree
>   :: StrucSig a b
>   -> ClusterTree (StrucSig a b) (Box a b)
> sigClusterTree (Definer _ c) = c
> sigClusterTree (Alias _ _ c) = c

Cluster trees allow flexible traversal, since they contain pointers
going both up and down.  The clusterTreeRoot function returns the
node at the root of a tree; this in turn points up to the black box
that owns the cluster.

> clusterTreeRoot
>   :: Signal a
>   => ClusterTree (StrucSig a b) (Box a b)
>   -> ClusterTree (StrucSig a b) (Box a b)
> clusterTreeRoot c =
>   case clusterParent c of
>     ParentCluster c' i -> clusterTreeRoot c'
>     BoxInCluster b i   -> c
>     BoxOutCluster b    -> c

Given a cluster tree node, returns the black box that owns the
cluster.  clupbox

> clusterTreeOwner
>   :: Signal a
>   => ClusterTree (StrucSig a b) (Box a b)
>   -> Box a b
> clusterTreeOwner c =
>   case clusterParent c of
>     ParentCluster c' i -> clusterTreeOwner c'
>     BoxInCluster b i   -> b
>     BoxOutCluster b    -> b


> outPortSource :: StrucSig a b -> StrucSig a b
> outPortSource (Alias x OutPort p) = x
> outPortSource (Alias x _ p) = error "not an OutPort"
> outPortSource _ = error "not an Alias"

> inPortSource :: StrucSig a b -> StrucSig a b
> inPortSource (Alias x InPort p) = x
> inPortSource (Alias x _ p) = error "not an InPort"
> inPortSource _ = error "not an Alias"

> boxinputs :: Signal a => Box a b -> [StrucSig a b]
> boxinputs b =
>   map inPortSource
>     (concat (map cluSigs (boxInPorts b)))


Showing Signals, Clusters and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several ways to show a signal (that is, produce a
descriptive character string).

> showcluster :: Int -> ClusterTree (StrucSig a b) (Box a b)
>    -> String
> showcluster i x =
>   "port " ++ show i ++ ": " ++ showCluNames x

> describe_sig :: StrucSig a b -> String
> describe_sig (Definer x p) = cluname p
> describe_sig (Alias x port p) = show port ++ " " ++ cluname p

> sigName :: Signal a => StrucSig a b -> String
> sigName = cluname . sigClusterTree

> describe_sig_in_box :: Signal a => StrucSig a b -> String
> describe_sig_in_box (Alias x InPort p) =
>   "\nsiginbox InPort " ++ cluname p
>   ++ "<-" ++ describe_sig x
>   ++ "(" ++ boxSpecies (sigOwner x) ++ ")"
> describe_sig_in_box (Alias x OutPort p) =
>   "\nsiginbox Local " ++ cluname p
>   ++ "<-" ++ describe_sig x

Use showHandle to print a box, given any of its output signals.
Use showboxfull to print the box, given that box itself, and
showboxquick to give a quick summary of the contents of the box.

> showHandle :: Signal a => StrucSig a b -> IO ()
> showHandle x = showboxfull 0 (sigOwner x)

> showboxquick :: Signal a => Int -> Box a b -> IO ()
> showboxquick i b =
>   do putStrLn (indent i ++ "Summary of " ++ boxSpecies b)
>      putStrLn (indent i ++ "  input ports:" ++
>             concat (map ((' ':) . showCluNames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  output ports: "
>                  ++ showCluNames (boxOutPorts b))

> showboxfull :: Signal a => Int -> Box a b -> IO ()
> showboxfull i b =
>   do putStrLn (indent i ++ "Black box " ++ boxSpecies b)
>      putStrLn (indent i ++ "  Input ports:" ++
>             concat (map ((' ':) . showCluNames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  Output ports: "
>                  ++ showCluNames (boxOutPorts b))
>      putStrLn (indent i ++ "  Interior signals:")
>      doall (\s -> putStrLn (indent i ++ "  " ++ describe_sig s))
>            (boxSignals b)
>      putStrLn (indent i ++ "  Interior circuits:")
>      doall (\b -> showboxfull (i+1) b) (boxChildren b)
  

Wires
~~~~~

> data Wire a = Wire

> {- data Wire a
>   = Unidir (Port a) [Port a]
>   | Bidir  [Port a] -}


Comparison of Signals and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(need updating) The boxEq function compares two boxes to determine
whether they are the same, assuming that they are siblings but
without examining their horizontal or vertical locations.  This
operation is required in order to determine the list of children
within the parent, which is needed in turn in order to work out the
horizontal locations of the children.

> boxEqInBox :: Signal a => Box a b -> Box a b -> Bool
> boxEqInBox b1 b2 =
>   let b1inps = boxinputs b1
>       b2inps = boxinputs b2
>   in boxSpecies b1 == boxSpecies b2
>      && and (zipWith sigEqInBox b1inps b2inps)

(need updating) The following function determines whether two
signals are the same.  The function assumes that they belong to the
same box but it does not use the horizontal or vertical locations.
Within a box b, two signals are the same if either they are both
(the same) input to b, or if they are both (the same) output from
the same box (which is a child of b), or if they are the same
internal signal within the box.

> sigEqInBox :: Signal a => StrucSig a b -> StrucSig a b -> Bool
> sigEqInBox (Definer _ _) (Definer _ _) =
>   error "both args are Definers"
> sigEqInBox (Definer _ _) (Alias _ _ _) =
>   error "first arg is Definer"
> sigEqInBox (Alias _ _ _) (Definer _ _) =
>   error "second arg is Definer"
> sigEqInBox (Alias a1 z1 p1) (Alias a2 z2 p2) =
>   case (z1,z2) of
>     (OutPort,OutPort) ->
>       case cmpCluLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> boxEqInBox b1 b2
>     (InPort,InPort) ->
>       case cmpCluLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> True
>     (Tag t1, Tag t2) -> t1==t2
>     (InPort,OutPort) -> False
>     (OutPort,InPort) -> False
>     (_,_) -> False

The cmpSigLoc function determines whether two signals have the same
location within a box, up to identity of the boxes providing the signals.
If the signals are definitely different, cmpSigLoc returns Nothing; if the
signals are provided by corresponding OutPorts of two boxes b1 and b2,
then it returns Just b1 b2 (and the signals are identical if and only if
boxes b1 and b2 are identical).

> cmpCluLoc
>   :: Signal a
>   => ClusterTree (StrucSig a b) (Box a b)
>   -> ClusterTree (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)

> cmpCluLoc (Singleton _ p1 _) (Singleton _ p2 _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Tuple2 _ p1 _ _) (Tuple2 _ p2 _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Tuple3 _ p1 _ _ _) (Tuple3 _ p2 _ _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Word _ p1 _ _) (Word _ p2 _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc _ _ = Nothing

> cmpParentLoc
>   :: Signal a
>   => ClusterParent (StrucSig a b) (Box a b)
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)

> cmpParentLoc (BoxOutCluster b1) (BoxOutCluster b2) =
>   Just (b1,b2)
> cmpParentLoc (BoxInCluster b1 i1) (BoxInCluster b2 i2) =
>   if i1==i2
>   then Just (b1,b2)
>   else Nothing
> cmpParentLoc (ParentCluster c1 i1) (ParentCluster c2 i2) =
>   if i1==i2
>   then cmpCluLoc c1 c2
>   else Nothing
> cmpParentLoc _ _ = Nothing



Discovering the contents of a box
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(old description, needs updating) The traceback function takes the
list of outputs from a black box, and it delivers a list of the
children of that box.  This list, in turn, is used to define the
horizontal locations of those children: a child's horizontal
location is its index in this list.  The function builds the list
by starting with the outputs, and tracing all the signals backwards
through the box, adding each newly discovered box to the
accumulator.  The traceback does not go through the internal
structure of a child; instead, it teleports directly from the
output of a child back to its inputs.  A branch of the traceback
terminates when an input to the box is encountered.  This function
assumes that the box circuit is a directed acyclic graph; if cycles
are present, the traceback will not terminate.

The first accumulator, xs, is the list of signals that remain to be
traced back.  The second accumulator, bs, is the list of children
discovered so far.  It is essential that each child appears exactly
once in the final result of traceback.

> traceback
>   :: Signal a
>   => [Int]
>   -> [StrucSig a b]
>   -> [Box a b]
>   -> [StrucSig a b]
>   -> ([Box a b], [StrucSig a b])

> traceback seen [] bs ss = (bs,ss)
> traceback seen (x:xs) bs ss =
>   case x of
>     Alias a z p ->
>       case z of
>         InPort -> traceback seen xs bs ss
>         Tag i ->
>           if member i seen
>             then traceback seen xs bs ss
>             else traceback (i:seen) (a:xs) bs ss
>         OutPort ->
>           if or (map (sigEqInBox x) ss)
>             then traceback seen xs bs ss
>             else let b = sigOwner x
>                  in if or (map (boxEqInBox b) bs)
>                       then traceback seen xs bs (ss++[x])
>                       else traceback seen (boxinputs b ++ xs)
>                              (bs++[b]) (ss++[x])
>         _ -> error "traceback: funny port, not In/Out port"
>     _ ->  error "Badly formed signal, not an alias"


Constructing Black Boxes for Primitive Components
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prim01Struc function builds a structural representation of a
primitive component that takes no inputs, and produces a singleton
output signal.  This is suitable for defining constant signals and
top level circuit inputs.  Its arguments are:

  x      :: a       = the constant value of the output
  ylab   :: String  = the label for this input signal
  boxlab :: c       = the species of box (e.g. "Input")
  layout :: b       = tag decorating the box

> prim01Struc
>   :: Signal a
>   => a -> String -> String -> b
>   -> StrucSig a b

> prim01Struc x ylab boxlab layout =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxInPorts  = []
>             , boxOutPorts = ytree
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       y    = Definer x undefined
>       yout = Alias y OutPort ytree
>       ytree = Singleton (Just ylab) (BoxOutCluster box) yout
>   in yout

The prim11 function builds a primitive black box circuit that takes one
input and produces one output.  The arguments have the following meanings:
f computes the semantics of the output; laba is the label for the input
signal a, labx is the label for the output signal x, sp is the box species
and tag allows arbitrary information to be attached to the circuit.

> prim11Struc
>   :: Signal a
>   => (a->a) -> String -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d

> prim11Struc f laba labx boxlab layout a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just laba) (BoxInCluster box 0) ai
>       x     = Definer (f (behavior a)) undefined
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just labx) (BoxOutCluster box) xout
>   in xout

A primitive that takes j singleton inputs and produces k singleton outputs
can be defined by a function whose name has the form primjkStruc.  These
functions have definitions similar to that of prim11Struc above.

> prim21Struc
>   :: Signal a
>   => (a->a->a) -> [String] -> String -> String -> b
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> prim21Struc f [laba,labb] labx boxlab layout a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ???ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just laba) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just labb) (BoxInCluster box 1) bi
>       x     = Definer (f (behavior a) (behavior b)) undefined
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just labx) (BoxOutCluster box) xout
>   in xout

> prim31Struc
>   :: Signal a
>   => (a->a->a->a) -> [String] -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d -> StrucSig a d -> StrucSig a d

> prim31Struc f [laba,labb,labc] labx boxlab layout a b c =
>   let box =
>         Box {boxSpecies   = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ??? ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just laba) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just labb) (BoxInCluster box 1) bi
>       ci    = Alias c InPort ctree
>       ctree = Singleton (Just labc) (BoxInCluster box 2) ci
>       x     = Definer (f (behavior a) (behavior b) (behavior c))
>                  undefined
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just labx) (BoxOutCluster box) xout
>   in xout


Standard Primitive Circuit Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constant signals

> conStruc :: Signal a => String -> a -> StrucSig a b
> conStruc l x = prim01Struc x l "Constant" undefined

Inputs to a circuit

> input :: Signal a => String -> a -> StrucSig a b
> input l x = prim01Struc x l "Input" undefined

The buffer and inverter

> bufStruc, invStruc
>   :: Signal a
>   => StrucSig a d -> StrucSig a d
> bufStruc = prim11Struc buf "a" "x" "buf" undefined
> invStruc = prim11Struc inv "a" "x" "inv" undefined

> dffStruc
>   :: (Signal a, Clocked a)
>   => StrucSig a b -> StrucSig a b
> dffStruc = prim11Struc dff "dff_ld" "dff_a" "dff" undefined


Basic logic gates

> and2Struc, or2Struc, xor2Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b
> and2Struc = prim21Struc and2
>               ["and2_a","and2b"] "and2_x" "and2" undefined
> or2Struc  = prim21Struc or2
>               ["or2_a","or2_b"] "or2_x" "or2"  undefined
> xor2Struc = prim21Struc xor2
>               ["xor2_a","xor2_b"] "xor2_x" "xor2" undefined

> and3Struc, or3Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b
> and3Struc = prim31Struc and3
>               ["and3_a","and3_b","and3_c"] "and3_x" "and3"
>               undefined
> or3Struc  = prim31Struc or3
>               ["or3_a","or3_b","or3_c"] "or3_x" "or3"  undefined


Signal Instances for Standard Structural Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> instance Signal a => Signal (StrucSig a d)
>   where
>  --   sglsig = undefined  -- ??? need this?
>     is0  = is0 . behavior
>     is1  = is1 . behavior
>     zero = conStruc "0" zero
>     one  = conStruc "1" one
>     buf  = bufStruc
>     inv  = invStruc
>     and2 = and2Struc
>     or2  = or2Struc
>     xor2 = xor2Struc
>     and3 = and3Struc
>     or3 = or3Struc
>     box11 = undefined --box11Struc
>     box21 = undefined -- box21Struc
>     box22 = undefined --box22Struc
>     box31 = undefined --box31Struc
>     box41 = undefined
> --     mkOut = undefined --mkOutStruct

> instance (Signal a, Clocked a) => Clocked (StrucSig a b) where
>   dff = dffStruc


> showCluRep :: Show a => ClusterTree (StrucSig a b) c -> String
> showCluRep (Singleton s _ x) = strucSigStruc x
> showCluRep (Tuple2 s p a b) =
>   "(" ++ showCluRep a ++ ", " ++ showCluRep b ++ ")"
> showCluRep _ = error "bad case"

> strucSigStruc :: Show a => StrucSig a b -> String
> strucSigStruc (Definer x _) = "Definer-" ++ show x
> strucSigStruc (Alias x p _) =
>    "Alias-" ++ show p ++ " " ++ strucSigStruc x

Yet Another Type Class Experiment (May 18, 2001)
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

idea: try to use type class Clusterable to generate applications of bit,
tuple2, etc, rather than building up the data structures directly.

> class Cluster a b c d where
>   buildCluster :: c -> Port -> ClusterParent (StrucSig a b) (Box a b)
>     -> d -> (d, ClusterTree (StrucSig a b) (Box a b))

> instance Cluster a b [Char] (StrucSig a b) where
>   buildCluster label port parent x = bit label port parent x

> instance (Cluster a b c e, Cluster a b d f)
>   => Cluster a b (c,d) (e,f) where
>    buildCluster (xlab,ylab) port parent (x,y) =
>      tuple2 "*"
>         (buildCluster xlab)
>         (buildCluster ylab)
>         port parent (x,y)

New Approach (May 14, 2001)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

> bit
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> StrucSig a b
>   -> (StrucSig a b, ClusterTree (StrucSig a b) (Box a b))

> bit label port parent x =
>   let x' = Alias x port clutree
>       clutree = Singleton (Just label) parent x'
>   in (x', clutree)

> tuple2
>   :: String
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, ClusterTree (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, ClusterTree (StrucSig a b) (Box a b)))
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (c,d)
>   -> ((c,d), ClusterTree (StrucSig a b) (Box a b))

> tuple2 label f g port parent (x,y) =
>   let tree = Tuple2 (Just label) parent xtree ytree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>   in ((x',y'), tree)

> tuple3
>   :: String
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, ClusterTree (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, ClusterTree (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> e -> (e, ClusterTree (StrucSig a b) (Box a b)))
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (c,d,e)
>   -> ((c,d,e), ClusterTree (StrucSig a b) (Box a b))

> tuple3 label f g h port parent (x,y,z) =
>   let tree = Tuple3 (Just label) parent xtree ytree ztree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>       (z',ztree) = h port (ParentCluster tree 1) z
>   in ((x',y',z'), tree)


Testing...   Move to SigStructTest when it's stable
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A circuit with singleton input and output clusters

> test1 :: Bool -> IO ()
> test1 xval =
>   let x = Definer xval inpclu
>       inpclu = Singleton (Just "input") (BoxInCluster undefined 0) x
>       (x',xt) = bit "x" InPort (BoxInCluster undefined 0) x
>       y = inv x
>       (y',yt) = bit "y" OutPort (BoxOutCluster undefined) y
>   in do putStrLn "Test singleton input and singleton output"
>         putStrLn ("x  = " ++ show (behavior x))
>         putStrLn ("x' = " ++ show (behavior x'))
>         putStrLn ("y  = " ++ show (behavior y))
>         putStrLn ("y' = " ++ show (behavior y'))
>         putStrLn ("Input source tree = " ++ showCluRep inpclu)
>         putStrLn ("Inport tree       = " ++ showCluRep xt)
>         putStrLn ("Outport tree      = " ++ showCluRep yt)
>         return ()

A circuit with a pair input and pair output

> test22a :: (Bool,Bool) -> IO ()
> test22a (xval,yval) =
>   let inclu = Tuple2 Nothing (BoxInCluster undefined 0)
>                 inclux incluy
>       inclux = Singleton (Just "inx") (ParentCluster inclu 0) inpx
>       incluy = Singleton (Just "iny") (ParentCluster inclu 1) inpy
>       inpx = Definer xval inclux
>       inpy = Definer yval incluy
>       inp = (inpx,inpy) -- the actual argument to the circuit
>       (inp',intree) =
>         tuple2 "inputs" (bit "a") (bit "b")
>           InPort (BoxInCluster undefined 0) inp
>       circuit (x,y) = (and2 x y, or2 x y)
>       out = circuit inp'
>       (out',outtree) =
>         tuple2 "outputs" (bit "x") (bit "y")
>           OutPort (BoxOutCluster undefined) out
>       (resultx,resulty) = out'
>   in do putStrLn "test22a: pair input and pair output "
>         putStrLn ("inpx = " ++ show (behavior inpx))
>         putStrLn ("inpy = " ++ show (behavior inpy))
>         putStrLn ("resultx = " ++ show (behavior resultx))
>         putStrLn ("resulty = " ++ show (behavior resulty))
>         putStrLn ("Input source tree = " ++ showCluRep inclu)
>         putStrLn ("Inport tree       = " ++ showCluRep intree)
>         putStrLn ("Outport tree      = " ++ showCluRep outtree)
>         return ()


Constructing Black Box Circuits with Unclustered Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> box11Struc
>   :: Signal a
>   => String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b

> box11Struc boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       x     = f ai
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just xlab) (BoxOutCluster box) xout
>       (bs,ps) = traceback [] [x] [] [ai]
>   in xout


Added this May 18, 2001... this is the real test of the new approach to
handling clusters!!!

The sources function takes an output cluster tree and returns the list of
underlying output signals, which can then be used to trace back through
the contents of a black box.  Note that each signal in the output cluster
tree has an OutPort alias, which must be skipped.

> outSources
>   :: Signal a
>   => ClusterTree (StrucSig a b) (Box a b)
>   -> [StrucSig a b]

> outSources (Singleton _ _ x) =
>   case x of
>     Alias x OutPort cp -> [x]
>     _ -> error "outSources: invalid OurPort alias"
> outSources (Tuple2 _ _ x y) =
>   outSources x ++ outSources y
> outSources (Tuple3 _ _ x y z) =
>   outSources x ++ outSources y ++ outSources z
> outSources _ = error "outSources: unimplemented case"

> boxcluStruc1
>   :: Signal a
>   => String
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>       -> c -> (c, ClusterTree (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>       -> d -> (d, ClusterTree (StrucSig a b) (Box a b)))
>   -> b
>   -> (c->d)
>   -> c
>   -> d

> boxcluStruc1 boxlab mka mkx layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       (a',atree) = mka InPort (BoxInCluster box 0) a
>       dummy1 = foobar a a'
>       x = f a'
>       (x',xtree) = mkx OutPort (BoxOutCluster box) x
>       dummy2 = foobar x x'
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x'

Sunday, May 20, 2001.  Pass the label tuples directly into the black box
building function, whereh it can use buildCluster itself, and try to avoid
the "ambiguous type" error message by passing a signal witness into the
function.

> boxcluStruc1new
>   :: (Signal a,
>       Cluster a b c d,
>       Cluster a b e f)
>   => StrucSig a b  -- signal witness
>   -> String   -- box label
>   -> c        -- input label
>   -> e        -- output label
>   -> b        -- layout
>   -> (d->f)   -- behavior
>   -> d        -- input
>   -> f        -- output

> boxcluStruc1new witness boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       foo = witness : ps
>       (a',atree) = buildCluster alab InPort (BoxInCluster box 0) a
>       x = f a'
>       (x',xtree) = buildCluster xlab OutPort (BoxOutCluster box) x
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x'

> circ22May20
>  :: Signal a
>  => StrucSig a ()
>  -> (StrucSig a (), StrucSig a ())
>  -> (StrucSig a (), StrucSig a ())

> circ22May20 witness =
>   let f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1new witness "circ22May20"
>         ("abd","def") ("ghi","jkl") () f


> foo :: Signal a =>
>   (StrucSig a (), StrucSig a ())
>   -> (StrucSig a (), StrucSig a ())

> foo = circ22May20 zero


above is the new approach fom May 20 ....

> circ22
>  :: Signal a
>   => (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())

> circ22 =
>   let inp = tuple2 "inputs" (bit "a") (bit "b")
>       inp2 = buildCluster ("aaa","bbb")
>       dummy1 = foobar inp inp2
>       out = tuple2 "outputs" (bit "x") (bit "y")
>       out2 = buildCluster ("xxx","yyy")
>       dummy2 = foobar out out2
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1 "circ22" inp2 out2 () f


> circ22d
>  :: Signal a
>   => (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())
> circ22d inputs =
>   let inp = tuple2 "inputs" (bit "a") (bit "b")
>       inp2 = buildCluster ("aaa","bbb")
>       dummy1 = foobar inp inp2
>       out = tuple2 "outputs" (bit "x") (bit "y")
>       out2 = buildCluster ("xxx","yyy")
>       dummy2 = foobar out out2
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1 "circ22" inp2 out2 () f inputs

> foobar :: a -> a -> ()
> foobar x y = ()


> {- circ22magic
>  :: (
>      Cluster a b ([Char],[Char]) (StrucSig a b, StrucSig a b)
>     )
>   => (StrucSig a b, StrucSig a b) -> (StrucSig a b, StrucSig a b)

> circ22magic =
>   let inp = buildCluster ("a","b")
>       out = buildCluster ("x","y")
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1 "circ22magic" inp out () f -}


> exper :: Cluster a b c d
>       => c -> Port -> ClusterParent (StrucSig a b) (Box a b)
>       -> d -> (d, ClusterTree (StrucSig a b) (Box a b))
> exper = buildCluster

> {-
> circ22e
>  :: (Signal a,
>      Signal (StrucSig a b),
>      Cluster a b [Char] (StrucSig a b),
>      Cluster a b ([Char],[Char]) (StrucSig a b, StrucSig a b))
>  => (StrucSig a b, StrucSig a b) -> (StrucSig a b, StrucSig a b)

> circ22e =
>   let inp = exper ("a","b")
>       out = exper ("x","y")
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1 "circ22magic" inp out () f
> -}


> test22b :: (Bool,Bool) -> IO ()
> test22b (a,b) =
>   do let inpa = Definer a (Singleton (Just "srca") undefined inpa)
>      let inpb = Definer b (Singleton (Just "srcb") undefined inpb)
>      let (x,y) = circ22 (inpa, inpb)
>      putStrLn "test22b: pair input, pair output, with clusters"
>      putStrLn ("input a = " ++ show (behavior inpa))
>      putStrLn ("input b = " ++ show (behavior inpb))
>      putStrLn ("result x = " ++ show (behavior x))
>      putStrLn ("result y = " ++ show (behavior y))
>      let bx = sigOwner x
>      showboxquick 0 bx
>      showboxfull 0 bx
>      return ()

It seems to work!!!

May 19, 2001, try to simplify the form of a circuit specification...

mkclu :: 


> {- boxcluStruc1new
>   :: Signal a
>   => String
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>       -> c -> (c, ClusterTree (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>       -> d -> (d, ClusterTree (StrucSig a b) (Box a b)))
>   -> b
>   -> (c->d)
>   -> c
>   -> d -}

> {- boxcluStruc1new
>   :: (Cluster a b c e, Cluster a b d f)
>   => String  -- box label
>   -> c       -- input cluster label
>   -> d       -- output cluster label
>   -> b       -- box layout type
>   -> (e->f)  -- circuit behavior
>   -> e       -- input cluster
>   -> f       -- output cluster

> boxcluStruc1new boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       dummy1 = foobar a a'
>       dummy2 = foobar x x'
>       dummy3 = foobar atree xtree
>       (a',atree) = buildCluster alab InPort (BoxInCluster box 0) a
>       x = f a'
>       (x',xtree) = buildCluster xlab OutPort (BoxOutCluster box) x
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x' -}


> {- circ22new
>  :: Signal a
>   => (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())

> circ22new =
>   let inp = tuple2 "inputs" (bit "a") (bit "b")
>       inp2 = buildCluster ("aaa","bbb")
>       dummy1 = foobar inp inp2
>       out = tuple2 "outputs" (bit "x") (bit "y")
>       out2 = buildCluster ("xxx","yyy")
>       dummy2 = foobar out out2
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1new "circ22" ("aaa","bbb") ("xxx","yyy") () f -}


> circ22c
>  :: Signal a
>   => (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())

> circ22c ab =
>   let inp = tuple2 "inputs" (bit "a") (bit "b")
>       inp2 = buildCluster ("aaa","bbb")
>       dummy1 = foobar inp inp2
>       out = tuple2 "outputs" (bit "x") (bit "y")
>       out2 = buildCluster ("xxx","yyy")
>       dummy2 = foobar out out2
>       f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1 "circ22" inp2 out2 () f ab


Another idea... (May 19th) maybe the problem is that currently I cannot
relate the underlying signal and layout types (a and b) when the two
clusters are built independently.  Possible solution: build them together!
Make a new Cluster class that builds two clusters simultaneoulsly!  When
there are several input clusters, that is another problem :-)


a - signal type
b - box layout type
c - input label type
d - output label type
e - input cluster type
f - output cluster type

class ClusterNew a b where
  buildCluster
    :: c
    -> d
    -> ClusterParent (StrucSig a b) (Box a b)
    -> 



....................

> box21Struc
>   :: Signal a
>   => String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box21Struc boxlab alab blab xlab layout f a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just blab) (BoxInCluster box 1) bi
>       x     = f ai bi
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just xlab) (BoxOutCluster box) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi]
>   in xout


> box22Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> (StrucSig a b, StrucSig a b)

> box22Struc boxlab alab blab xlab ylab layout f a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = outtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just blab) (BoxInCluster box 1) bi
>       (x,y) = f ai bi
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just xlab) (ParentCluster outtree 0) xout
>       yout  = Alias y OutPort ytree
>       ytree = Singleton (Just ylab) (ParentCluster outtree 1) yout
>       outtree = Tuple2 Nothing (BoxOutCluster box) xtree ytree
>       (bs,ps) = traceback [] [x,y] [] [ai,bi]
>   in (xout,yout)

> box31Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b  -> StrucSig a b
>       -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box31Struc boxlab alab blab clab xlab layout f a b c =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just blab) (BoxInCluster box 1) bi
>       ci    = Alias c InPort ctree
>       ctree = Singleton (Just clab) (BoxInCluster box 2) ci
>       x     = f ai bi ci
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just xlab) (BoxOutCluster box) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi,ci]
>   in xout

> box32Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>     -> (StrucSig a b, StrucSig a b)

> box32Struc boxlab alab blab clab xlab ylab layout f a b c =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = outtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       bi    = Alias b InPort btree
>       btree = Singleton (Just blab) (BoxInCluster box 1) bi
>       ci    = Alias c InPort ctree
>       ctree = Singleton (Just clab) (BoxInCluster box 2) ci
>       (x,y) = f ai bi ci
>       xout  = Alias x OutPort xtree
>       yout  = Alias y OutPort ytree
>       xtree = Singleton (Just xlab) (ParentCluster outtree 0) xout
>       ytree = Singleton (Just ylab) (ParentCluster outtree 1) yout
>       outtree = Tuple2 Nothing (BoxOutCluster box) xtree ytree
>       (bs,ps) = traceback [] [x,y] [] [ai,bi,ci]
>   in (xout,yout)



======================================================================
SigStructExtra

Generic Structural Composition of Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The box function can be used to define a black box circuit.  It's
intended to be fairly lightweight, so that there isn't too much
notational overhead required to define a new circuit by connecting
several smaller circuits together.

> {- 

> box
>   :: BoxType
>   -> CircLabel
>   -> [(StrucSig a, String)]
>   -> [(StrucSig a, String)]
>   -> ([StrucSig a], [StrucSig a])

> box t l xs ys =
>   let box = Box
>        {boxType = t, boxInst = l, boxLoc = loc, boxInputs = inps,
>         boxChildren = cs, boxInternal = zs, boxOutputs = outs}
>     --  inps, outs, zs :: [StrucSig]
>       inps = zipWith (\i (x,l) -> mkInport box i x l) [0..] xs
>	outs = zipWith (\i (y,l) -> mkOutport box i y l) [0..] ys
>       loc = getLoc box xs
>       cs = traceback [] (map fst ys) []
>       zs = []
>   in (inps, outs)

The input signals to the circuit are c_, x_ and y_.

> getLoc :: Box a -> [(StrucSig a, String)] -> BoxLoc a
> getLoc b [] = TopLevel
> getLoc b ((x,l):xs) =
>   let p = owner x
>       d = boxDepth p
>       siblings = boxChildren p
>          -- search b's children for self
>       i = findIndex b i siblings
>   in Child d b i

> findIndex :: Box a -> Int -> [Box a] -> Int
> findIndex b i [] = error "Cannot find myself in my own family!"
> findIndex b i (x:xs) =
>   if boxEq b x then i else findIndex b (i+1) xs

> boxDepth :: Box a -> Int
> boxDepth b =
>   case boxLoc b of
>     TopLevel -> 0
>     Child d p i -> d

> mkInport :: Box a -> Int -> StrucSig a -> String -> StrucSig a
> mkInport b i x l =
>  Inport (altsem x) x
>    (SigLoc {sigOwner=b, sigSrc=Input, sigIdx=i, sigLabel=l})

> mkOutport :: Box a -> Int -> StrucSig a -> String -> StrucSig a
> mkOutport b i x l =
>  Outport (altsem x) x
>    (SigLoc {sigOwner=b, sigSrc=Output, sigIdx=i, sigLabel=l})

> mkOutPrim :: a -> Box a -> Int -> String -> StrucSig a
> mkOutPrim x b i l =
>  OutPrim x (SigLoc {sigOwner=b, sigSrc=Output, sigIdx=i, sigLabel=l})

> -}


Constructing Black Box Circuits with Clustered Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> mkOutStruct
>   :: Signal a
>   => String
>   -> [CluSpec (StrucSig a d)]
>   -> CluSpec (StrucSig a d)
>   -> Cluster (StrucSig a d) d

> mkOutStruct boxn xs y = undefined --???
> {-   let box = Box
>         {boxSpecies = boxn,
>          boxTag = undefined,
>          boxChildren = [],  -- todo
>          boxInPorts = inps,
>          boxInternal = [],  -- todo
>          boxOutPorts = out}
>       inps = zipWith g xs [0..]
>       g x i = mkCluster TopCluster x
>       out = mkCluster TopCluster y
>   in out -}


{-
Black Boxes for Combinators
~~~~~~~~~~~~~~~~~~~~~~~~~~~

maybe this belongs in another module, perhaps a module that
defines signal types of the form StructSig a b, while this
module just defines the tools for such signals...

cmscanrStruct :: (a->b->(b,c)) -> b -> [a] -> (b,[c])
cmscanrStruct f a xs =
  let (a',ys) = 
-}




======================================================================
labelledcircuits
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

> {-
> halfAdd :: (Signal a, Structured a) => a -> a -> (a,a)
> halfAdd =
>   box22 "halfAdd" "x" "y" "sum2" ("c","s") undefined f
>   where f x y = (and2 x y, xor2 x y)
> -}

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

> test_RipAddS =
>   let a = False :: Bool
>       xys = [(False,False), (False,True),
>              (True,True),   (True,False)]
>            -- 0011=3, 0110=6, result = 9 = 1001 carry 0
>       css = rippleAddS a xys
>   in css
> -}


======================================================================
SigStructHold
> {- rest of the file


----------------------------------------------------------------------
Experiments that don't work...

Experiment: Try using functional dependencies to avoid needing the
witness...

In bar, the idea was to obtain the witness from an input.  Maybe it is
possible to do this via a new class operation?  Doubtful!  But try it...

> {-
> class Cluster a b c d => CluSig a b c d | d -> a where
>   getCluSig :: c -> d -> StrucSig a b
> 
> instance CluSig a b c (StrucSig a b) where
>   getCluSig _ x = x
> 
> instance (CluSig a b c d, CluSig a b e f)
>     => CluSig a b (c,e) (d,f) where
>   getCluSig lab (x,y) = getCluSig lab x
> -}

> boxcluStruc1newFunDep
>   :: (Signal a b,
>       Cluster a b c d,
>       Cluster a b e f)
>   => String   -- box label
>   -> c        -- input label
>   -> e        -- output label
>   -> b        -- layout
>   -> (d->f)   -- behavior
>   -> d        -- input
>   -> f        -- output

> boxcluStruc1newFunDep boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>  --     witness = getCluSig a : ps
>       (a',atree) = buildCluster alab InPort (BoxInCluster box 0) a
>       x = f a'
>       (x',xtree) = buildCluster xlab OutPort (BoxOutCluster box) x
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x'


> {- circ22fundep
>  :: Signal a b
>  => (StrucSig a (), StrucSig a ())
>  -> (StrucSig a (), StrucSig a ())

> circ22fundep =
>   let f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1newFunDep "circ22"
>         ("abd","def") ("ghi","jkl") () f -}

> bar :: Signal a () =>
>   (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())
> bar (p,q) =
>   let f (a,b) = (and2 a b, or2 a b)
>       bb = boxcluStruc1new witness "circ22"
>             ("abd","def") ("ghi","jkl") () f
>       witness = p
>   in bb (p,q)


> {-  baz :: Signal a b =>
>   (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())
> baz inps@(p,q) =
>   let f (a,b) = (and2 a b, or2 a b)
>       bb = boxcluStruc1new witness "circ22"
>             ("abd","def") ("ghi","jkl") () f
>       witness = getCluSig inps
>   in bb (p,q) -}

No, baz does not work...

Reading file "SigStruct.lhs":
ERROR "SigStruct.lhs" (line 981): Cannot justify constraints in
 explicitly typed binding
*** Expression    : baz
*** Type          : Signal a b => (StrucSig a (),StrucSig a ())
    -> (StrucSig a (),StrucSig a ())
*** Given context : Signal a b
*** Constraints   : (Signal b,
   Cluster b () [Char] (StrucSig a ()), CluSig b () (StrucSig a ()))

SigBool> 

Another idea.  Specify the witness explicitly as StrucSig a b, but leave
the base type unspecified...

> {- cruft :: Signal a b =>
>   (StrucSig a (), StrucSig a ()) -> (StrucSig a (), StrucSig a ())
> cruft (p,q) =
>   let f (a,b) = (and2 a b, or2 a b)
>       bb = boxcluStruc1new witness "circ22"
>             ("abd","def") ("ghi","jkl") () f
>       witness = Definer undefined undefined
>   in bb (p,q) -}

That was no good.

Next idea.  This is analogous to needing a top level default, like what is
the type of []...  maybe use dynamically scoped witness?  Would actually
be fairly attractive.  (Another possibility would be existential type for
base signal type.)


> {- circ22witless
>  :: Signal a b
>  => (StrucSig a (), StrucSig a ())
>  -> (StrucSig a (), StrucSig a ()) -}

> {- circ22witless =
>   let f (a,b) = (and2 a b, or2 a b)
>       wit = Definer True undefined
>       circ = boxcluStruc1new zero "circ22"
>                ("abd","def") ("ghi","jkl") () f
>   in circ -}

> -} end huge comment



======================================================================
SigStructSave.lhs

Showing Signals, Clusters and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several ways to show a signal (that is, produce a
descriptive character string).

> showcluster :: Int -> Cluster (StrucSig a b) (Box a b) -> String
> showcluster i x =
>   "port " ++ show i ++ ": " ++ showclunames x

> describe_sig :: StrucSig a b -> String
> describe_sig (Definer p x)
>   = describe_cluster_parent p
> describe_sig (Alias port p x)
>   = show port ++ " " ++ describe_cluster_parent p

> sigName :: Signal a => StrucSig a b -> String
> sigName x =
>   case sigParent x of
>     ParentCluster c i -> cluname c ++ "[" ++ show i ++ "]"
>     TopCluster b i -> "?["++ show i ++ "]"
>     UnknownLocation -> "?"

get rid of this soon?

> showboxInputs b =
>   "inputs to " ++ boxSpecies b ++ ": " ++ "??? TO DO ???"
> {-  ++ concat (map (++"*")
>       (map describe_sig_in_box (boxInPorts b))) -}

> describe_sig_in_box :: Signal a => StrucSig a b -> String
> describe_sig_in_box (Alias InPort p x) =
>   "\nsiginbox InPort " ++ describe_cluster_parent p
>   ++ "<-" ++ describe_sig x
>   ++ "(" ++ boxSpecies (sigOwner x) ++ ")"
> describe_sig_in_box (Alias OutPort p x) =
>   "\nsiginbox Local " ++ describe_cluster_parent p
>   ++ "<-" ++ describe_sig x

Use showHandle to print a box, given any of its output signals.
Use showboxfull to print the box, given that box itself, and
showboxquick to give a quick summary of the contents of the box.

> showHandle :: Signal a => StrucSig a b -> IO ()
> showHandle x = showboxfull 0 (sigOwner x)

> showboxquick :: Signal a => Int -> Box a b -> IO ()
> showboxquick i b =
>   do putStrLn (indent i ++ "Summary of " ++ boxSpecies b)
>      putStrLn (indent i ++ "  input ports:" ++
>             concat (map ((' ':) . showclunames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  output ports: "
>                  ++ showclunames (boxOutPorts b))

> showboxfull :: Signal a => Int -> Box a b -> IO ()
> showboxfull i b =
>   do putStrLn (indent i ++ "Black box " ++ boxSpecies b)
>      putStrLn (indent i ++ "  Input ports:" ++
>             concat (map ((' ':) . showclunames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  Output ports: "
>                  ++ showclunames (boxOutPorts b))
>      putStrLn (indent i ++ "  Interior signals:")
>      doall (\s -> putStrLn (indent i ++ "  " ++ describe_sig s))
>            (boxSignals b)
>      putStrLn (indent i ++ "  Interior circuits:")
>      doall (\b -> showboxfull (i+1) b) (boxChildren b)
  

Wires
~~~~~

> data Wire a = Wire

> {- data Wire a
>   = Unidir (Port a) [Port a]
>   | Bidir  [Port a] -}


Comparison of Signals and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(need updating) The boxEq function compares two boxes to determine
whether they are the same, assuming that they are siblings but
without examining their horizontal or vertical locations.  This
operation is required in order to determine the list of children
within the parent, which is needed in turn in order to work out the
horizontal locations of the children.

> boxEqInBox :: Signal a => Box a b -> Box a b -> Bool
> boxEqInBox b1 b2 =
>   let b1inps = boxinputs b1
>       b2inps = boxinputs b2
>   in boxSpecies b1 == boxSpecies b2
>      && and (zipWith sigEqInBox b1inps b2inps)

(need updating) The following function determines whether two
signals are the same.  The function assumes that they belong to the
same box but it does not use the horizontal or vertical locations.
Within a box b, two signals are the same if either they are both
(the same) input to b, or if they are both (the same) output from
the same box (which is a child of b), or if they are the same
internal signal within the box.

> sigEqInBox :: Signal a => StrucSig a b -> StrucSig a b -> Bool
> sigEqInBox (Definer _ _) (Definer _ _) =
>   error "both args are Definers"
> sigEqInBox (Definer _ _) (Alias _ _ _) =
>   error "first arg is Definer"
> sigEqInBox (Alias _ _ _) (Definer _ _) =
>   error "second arg is Definer"
> sigEqInBox (Alias z1 p1 a1) (Alias z2 p2 a2) =
>   case (z1,z2) of
>     (OutPort,OutPort) ->
>       case cmpSigLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> boxEqInBox b1 b2
>     (InPort,InPort) ->
>       case cmpSigLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> True
>     (Tag t1, Tag t2) -> t1==t2
>     (InPort,OutPort) -> False
>     (OutPort,InPort) -> False
>     (_,_) -> False

> cmpSigLoc
>   :: Signal a
>   => ClusterParent (StrucSig a b) (Box a b)
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)
> cmpSigLoc (TopCluster b1 i1) (TopCluster b2 i2) =
>   if i1==i2
>   then Just (b1,b2)
>   else Nothing
> cmpSigLoc (ParentCluster c1 i1) (ParentCluster c2 i2) =
>   if i1==i2
>   then cmpCluLoc c1 c2
>   else Nothing
> cmpSigLoc _ _ = Nothing

> cmpCluLoc
>   :: Signal a
>   => Cluster (StrucSig a b) (Box a b)
>   -> Cluster (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)
> cmpCluLoc (Singleton _ p1 _) (Singleton _ p2 _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Tuple2 _ p1 _ _) (Tuple2 _ p2 _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Tuple3 _ p1 _ _ _) (Tuple3 _ p2 _ _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Word _ p1 _ _) (Word _ p2 _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc _ _ = Nothing


Discovering the contents of a box
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(old description, needs updating) The traceback function takes the
list of outputs from a black box, and it delivers a list of the
children of that box.  This list, in turn, is used to define the
horizontal locations of those children: a child's horizontal
location is its index in this list.  The function builds the list
by starting with the outputs, and tracing all the signals backwards
through the box, adding each newly discovered box to the
accumulator.  The traceback does not go through the internal
structure of a child; instead, it teleports directly from the
output of a child back to its inputs.  A branch of the traceback
terminates when an input to the box is encountered.  This function
assumes that the box circuit is a directed acyclic graph; if cycles
are present, the traceback will not terminate.

The first accumulator, xs, is the list of signals that remain to be
traced back.  The second accumulator, bs, is the list of children
discovered so far.  It is essential that each child appears exactly
once in the final result of traceback.

> traceback
>   :: Signal a
>   => [Int]
>   -> [StrucSig a b]
>   -> [Box a b]
>   -> [StrucSig a b]
>   -> ([Box a b], [StrucSig a b])

> traceback seen [] bs ss = (bs,ss)
> traceback seen (x:xs) bs ss =
>   case x of
>     Alias z p a ->
>       case z of
>         InPort -> traceback seen xs bs ss
>         Tag i ->
>           if member i seen
>             then traceback seen xs bs ss
>             else traceback (i:seen) (a:xs) bs ss
>         OutPort ->
>           if or (map (sigEqInBox x) ss)
>             then traceback seen xs bs ss
>             else let b = sigOwner x
>                  in if or (map (boxEqInBox b) bs)
>                       then traceback seen xs bs (ss++[x])
>                       else traceback seen (boxinputs b ++ xs)
>                              (bs++[b]) (ss++[x])
>         _ -> error "traceback: funny port, not In/Out port"
>     _ ->  error "Badly formed signal, not an alias"


Constructing Black Boxes for Primitive Components
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prim01Struc function builds a structural representation of a
primitive component that takes no inputs, and produces a singleton
output signal.  This is suitable for defining constant signals and
top level circuit inputs.  Its arguments are:

  x   :: a  = the alternative semantics of the input signal.
  l   :: b  = the label for this input signal.
  sp  :: c  = the species of box (e.g. "Input")
  tag :: d  = tag decorating the box

> prim01Struc
>   :: (Signal a)
>   => a -> String -> String -> d
>   -> StrucSig a d

> prim01Struc x l sp tag =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = []
>             , boxOutPorts = yclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       y    = Definer (ParentCluster yclu 0) x
>       yout = Alias OutPort (ParentCluster yclu 0) y
>       yclu = Singleton (Just l) (TopCluster box 0) yout
>   in yout

The prim11 function builds a primitive black box circuit that takes
one input and produces one output.  The arguments have the
following meanings: f computes the semantics of the output; laba is
the label for the input signal a, labx is the label for the output
signal x, sp is the box species and tag allows arbitrary
information to be attached to the circuit.

> prim11Struc
>   :: (Signal a)
>   => (a->a) -> String -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d

> prim11Struc f laba labx sp tag a =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       x    = Definer (ParentCluster xclu 0)
>                (f (altsem a))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 1) xout
>   in xout

A primitive that takes j singleton inputs and produces k singleton
outputs can be defined by a function whose name has the form
primjkStruc.  These functions have definitions similar to that of
prim11Struc above.

> prim21Struc
>   :: (Signal a)
>   => (a->a->a) -> [String] -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d -> StrucSig a d

> prim21Struc f [laba,labb] labx sp tag a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       bi   = Alias InPort (ParentCluster bclu 0) b
>       bclu = Singleton (Just labb) (TopCluster box 1) bi
>       x    = Definer (ParentCluster xclu 0)
>               (f (altsem a) (altsem b))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 2) xout
>   in xout

> prim31Struc
>   :: (Signal a)
>   => (a->a->a->a) -> [String] -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d -> StrucSig a d -> StrucSig a d

> prim31Struc f [laba,labb,labc] labx sp tag a b c =
>   let box =
>         Box {boxSpecies   = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       bi   = Alias InPort (ParentCluster bclu 0) b
>       bclu = Singleton (Just labb) (TopCluster box 1) bi
>       ci   = Alias InPort (ParentCluster bclu 0) c
>       cclu = Singleton (Just labc) (TopCluster box 2) ci
>       x    = Definer (ParentCluster xclu 0)
>               (f (altsem a) (altsem b) (altsem c))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 3) xout
>   in xout


Standard Primitive Circuit Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constant signals

> conStruc :: Signal a => String -> a -> StrucSig a b
> conStruc l x = prim01Struc x l "Constant" undefined

Inputs to a circuit

> input :: Signal a => String -> a -> StrucSig a b
> input l x = prim01Struc x l "Input" undefined

The buffer and inverter

> bufStruc, invStruc
>   :: Signal a
>   => StrucSig a d -> StrucSig a d
> bufStruc = prim11Struc buf "a" "x" "buf" undefined
> invStruc = prim11Struc inv "a" "x" "inv" undefined

> dffStruc
>   :: (Signal a, Clocked a)
>   => StrucSig a b -> StrucSig a b
> dffStruc = prim11Struc dff "dff_ld" "dff_a" "dff" undefined

Basic logic gates

> and2Struc, or2Struc, xor2Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b
> and2Struc = prim21Struc and2
>               ["and2_a","and2b"] "and2_x" "and2" undefined
> or2Struc  = prim21Struc or2
>               ["or2_a","or2_b"] "or2_x" "or2"  undefined
> xor2Struc = prim21Struc xor2
>               ["xor2_a","xor2_b"] "xor2_x" "xor2" undefined

> and3Struc, or3Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b
> and3Struc = prim31Struc and3
>               ["and3_a","and3_b","and3_c"] "and3_x" "and3"
>               undefined
> or3Struc  = prim31Struc or3
>               ["or3_a","or3_b","or3_c"] "or3_x" "or3"  undefined


Signal Instances for Standard Structural Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> instance Signal a => Signal (StrucSig a d)
>   where
>     sglsig = undefined  -- ??? need this?
>     is0  = is0 . altsem
>     is1  = is1 . altsem
>     zero = conStruc "0" zero
>     one  = conStruc "1" one
>     buf  = bufStruc
>     inv  = invStruc
>     and2 = and2Struc
>     or2  = or2Struc
>     xor2 = xor2Struc
>     and3 = and3Struc
>     or3 = or3Struc
>     box11 = undefined --box11Struc
>     box21 = undefined -- box21Struc
>     box22 = undefined --box22Struc
>     box31 = undefined --box31Struc
>     box41 = undefined
> --     mkOut = undefined --mkOutStruct

> instance (Signal a, Clocked a) => Clocked (StrucSig a b) where
>   dff = dffStruc



Tuple/Cluster Conversion
~~~~~~~~~~~~~~~~~~~~~~~~

The LabClu class is used to convert a cluster of strings from
Haskell tuple types to the explicit Cluster data structure, which
can then be used to attach the labels to signals in the Clusterable
class.

> class LabClu a c where
>   labClu :: a -> Cluster String c

> instance LabClu String c where
>   labClu s = Singleton (Just s) undefined undefined

> instance (LabClu a c, LabClu b c) => LabClu (a,b) c  where
>   labClu (x,y) = Tuple2 Nothing undefined (labClu x) (labClu y)

> instance (LabClu a d, LabClu b d, LabClu c d)
>   => LabClu (a,b,c) d where
>   labClu (x,y,z) =
>     Tuple3 Nothing undefined (labClu x) (labClu y) (labClu z)

Conversion between nested tuples and lists and the explicit cluster
representation.  The underlying signal type is a, the actual cluster
type is b, and the structural tag type is c.  For example, we could
have a = Bool, b = (Bool,(Bool,Bool)) and c = ().


The barbaz function can be used to force the two clusters to be
constructed from the same underlying signal and box tag types.

> barbaz :: Cluster a b -> Cluster a b -> ()
> barbaz x y = ()

try a simpler clustering mechanism there is no conversion from a
Cluster representation to a cluster; all conversions go from the
real cluster (nested tuples) to the cluster rep.  The mkAlias
function builds a new cluster that is isomorphic to the old one,
with InPort/OutPort alias in place.  The mkCluRep operation builds
the cluster representation tree which can be used to find out where
signals are.

> class ClusterableTwo a where
>   mkAlias :: Port -> a -> a

> instance ClusterableTwo (StrucSig a b) where
>   mkAlias port x = Alias port undefined x

> instance (ClusterableTwo a, ClusterableTwo b)
>  => ClusterableTwo (a,b) where
>  mkAlias port (x,y) = (mkAlias port x, mkAlias port y)

> putLabel
>   :: ClusterParent a c
>   -> Cluster String c
>   -> Cluster a c
>   -> Cluster a c

> putLabel cp (Singleton _ _ s) (Singleton _ _ x) =
>   Singleton (Just s) cp x

> putLabel cp (Tuple2 _ _ a b) (Tuple2 _ _ x y) =
>   let r = Tuple2 Nothing cp
>             (putLabel (ParentCluster r 0) a x)
>             (putLabel (ParentCluster r 1) b y)
>   in r

> showCluRep :: Show a => Cluster (StrucSig a b) c -> String
> showCluRep (Singleton s _ x) = strucSigStruc x
> showCluRep (Tuple2 s p a b) =
>   "(" ++ showCluRep a ++ ", " ++ showCluRep b ++ ")"
> showCluRep _ = error "bad case"

> strucSigStruc :: Show a => StrucSig a b -> String
> strucSigStruc (Definer _ x) = "Definer-" ++ show x
> strucSigStruc (Definer3 _ x) = "Definer-" ++ show x
> strucSigStruc (Alias p _ x) =
>    "Alias-" ++ show p ++ " " ++ strucSigStruc x
> strucSigStruc (Alias3 p _ x) =
>    "Alias3-" ++ show p ++ " " ++ strucSigStruc x

> class CluFlat a b c where
>   cluit :: a -> Cluster b c

> instance CluFlat (StrucSig a b) (StrucSig a b) c where
>   cluit x = Singleton undefined undefined x

> instance (CluFlat a t c, CluFlat b t c) => CluFlat (a,b) t c where
>   cluit (x,y) = Tuple2 Nothing undefined (cluit x) (cluit y)


> testsigA   = Definer UnknownLocation True  :: StrucSig Bool ()
> testsigB   = Definer UnknownLocation False :: StrucSig Bool ()
> testpairAB = (testsigA, testsigB)

> test1 =
>   do putStrLn "Test mkAlias method (of class ClusterableTwo) on singleton"
>      let x = mkAlias InPort testsigA
>      let y = mkAlias OutPort testsigA
>      putStrLn (strucSigStruc testsigA)
>      putStrLn (strucSigStruc x)
>      putStrLn (strucSigStruc y)

> test7 =
>   do putStrLn "asdf"
>      let a = cluit testsigA :: Cluster (StrucSig Bool ()) ()
>      putStrLn (showCluRep a)
>      let b = cluit (testsigA,testsigB) :: Cluster (StrucSig Bool ()) ()
>      putStrLn (showCluRep b)
>      return ()

> bboxtest1 alab xlab f a =
>   do let alabclu = labClu alab
>      let a' = mkAlias InPort a
>      let a'rep = cluit a'  :: Cluster (StrucSig Bool ()) ()
>      let a'' = putLabel UnknownLocation alabclu a'rep
>                   :: Cluster (StrucSig Bool ()) ()
>      putStrLn ("a'' = " ++ showCluRep a'')
>      let x = f a'
>      let x' = mkAlias OutPort x
>      let xlabclu = labClu xlab
>      let x'rep = cluit x' :: Cluster (StrucSig Bool ()) ()
>      let x'' = putLabel UnknownLocation xlabclu x'rep
>      putStrLn ("x'' = " ++ showCluRep x'')
>      let clus = [a'',x'']
>      putStrLn "bboxtest1"
>      return ()

> test8 = bboxtest1 "abc" "def" inv testsigA
> test9 = bboxtest1 ("ab","cd") ("ef","gh") ha (testsigA,testsigB)

> ha (x,y) = (and2 x y, or2 x y)

q is the tuple type, a is the signal semantics type, b is the box
tag type.

> class ClusterableThree a b q where
>   mkAlias3
>     :: Port
>     -> LabTree
>     -> ClusterParent (StrucSig a b) (Box a b)
>     -> q
>     -> (q, Cluster (StrucSig a b) (Box a b))

> instance ClusterableThree a b (StrucSig a b) where
>   mkAlias3 port label cp x =
>     let x' = Alias3 port c x
>         c = Singleton lab cp x'
>         lab = case label of
>                 LabTree1 s -> s
>                 _ -> Just "bad singleton label"
>     in (x',c)

> instance (ClusterableThree a b x, ClusterableThree a b y)
>   => ClusterableThree a b (x,y) where
>   mkAlias3 port label cp (x,y) =
>     let (x',cx) = mkAlias3 port labx (ParentCluster p 0) x
>         (y',cy) = mkAlias3 port laby (ParentCluster p 1) y
>         p  = Tuple2 lab cp cx cy
>         (lab,labx,laby) =
>           case label of
>             LabTree2 lab labx laby -> (lab,labx,laby)
>             _ -> (Just "bad pair label 1",
>                   LabTree1 (Just "bad pair label 2"),
>                   LabTree1 (Just "bad pair label 3"))
>     in ((x',y'), p)

> class ClusterableFour a where
>   mkalias4
>     :: Port
>     -> LabTree
>     -> ClusterParent (StrucSig Bool ()) (Box Bool ())
>     -> a
>     -> (a, Cluster (StrucSig Bool ()) (Box Bool ()))

> instance ClusterableFour (StrucSig Bool ()) where
>   mkalias4 port label cp x =
>     let x' = Alias3 port c x
>         c = Singleton lab cp x'
>         lab = case label of
>                 LabTree1 s -> s
>                 _ -> Just "bad singleton label"
>     in (x',c)
  
> data LabTree
>   = LabTree1 (Maybe String)
>   | LabTree2 (Maybe String) LabTree LabTree
>   | LabTree3 (Maybe String) LabTree LabTree LabTree

> {- bboxtest2
>   :: (ClusterableThree a b p, ClusterableThree a b q)
>   => LabTree
>   -> LabTree
>   -> (p->q)
>   -> p -> q -}

> bboxtest2 alab xlab f a =
>   do let (a',atree) = mkAlias3 InPort alab UnknownLocation a
>      let x = f a'
>      let dummy1 = foobar a a'
>      let dummy2 = foobar x a'
>      let (x',xtree) = mkAlias3 OutPort xlab UnknownLocation x
>  --    putStrLn (showCluRep atree)
>  --    putStrLn (showCluRep xtree)
>      let clus = [atree,xtree]
>                  :: [Cluster (StrucSig Bool ()) (Box Bool ())]
>      putStrLn (show (altsem a))
>      putStrLn (show (altsem x))
>      return ()

> test10 =
>   bboxtest2
>     (LabTree1 (Just "a"))
>     (LabTree1 (Just "x"))
>     (inv :: StrucSig Bool () -> StrucSig Bool ())
>     testsigA


> bboxtest3 mkalias alab xlab f a =
>   do putStr "bboxtest3"
>      let (a',atree) = mkalias InPort alab UnknownLocation a
>      putStrLn ("input cluster = " ++ showCluRep atree)
>      let x = f a'
>      let (x',xtree) = mkalias OutPort xlab UnknownLocation x
>      return ()

> test11 =
>   bboxtest3
>     mkalias4
>     (LabTree1 (Just "a"))
>     (LabTree1 (Just "x"))
>     (inv :: StrucSig Bool () -> StrucSig Bool ())
>     testsigA

New Approach (May 14, 2001)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

> bit
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> StrucSig a b
>   -> (StrucSig a b, Cluster (StrucSig a b) (Box a b))

> bit label port parent x =
>   let x' = Alias3 port clutree x
>       clutree = Singleton (Just label) parent x'
>   in (x', clutree)

> tuple2
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, Cluster (StrucSig a b) (Box a b)))
>   -> (c,d)
>   -> ((c,d), Cluster (StrucSig a b) (Box a b))

> tuple2 label port parent f g (x,y) =
>   let tree = Tuple2 (Just label) parent xtree ytree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>   in ((x',y'), tree)

> tuple3
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> e -> (e, Cluster (StrucSig a b) (Box a b)))
>   -> (c,d,e)
>   -> ((c,d,e), Cluster (StrucSig a b) (Box a b))

> tuple3 label port parent f g h (x,y,z) =
>   let tree = Tuple3 (Just label) parent xtree ytree ztree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>       (z',ztree) = h port (ParentCluster tree 1) z
>   in ((x',y',z'), tree)


A circuit with singleton input and output clusters

> test12 :: Bool -> IO ()
> test12 xval =
>   let x = Definer3 inpclu xval
>       inpclu = Singleton (Just "input") (TopCluster undefined 0) x
>       (x',xt) = bit "x" InPort (TopCluster undefined 0) x
>       y = inv x
>       (y',yt) = bit "y" OutPort (TopCluster undefined 1) y
>   in do putStrLn "test12"
>         putStrLn ("x  = " ++ show (altsem x))
>         putStrLn ("x' = " ++ show (altsem x'))
>         putStrLn ("y  = " ++ show (altsem y))
>         putStrLn ("y' = " ++ show (altsem y'))
>         putStrLn ("Input source tree = " ++ showCluRep inpclu)
>         putStrLn ("Inport tree       = " ++ showCluRep xt)
>         putStrLn ("Outport tree      = " ++ showCluRep yt)
>         return ()

A circuit with a pair input and pair output

> foocirc (x,y) = (and2 x y, or2 x y)

> test13 :: (Bool,Bool) -> IO ()
> test13 (xval,yval) =
>   let inclu = Tuple2 Nothing (TopCluster undefined 0)
>                 inclux incluy
>       inclux = Singleton (Just "inx") (ParentCluster inclu 0) inpx
>       incluy = Singleton (Just "iny") (ParentCluster inclu 1) inpy
>       inpx = Definer3 inclux xval
>       inpy = Definer3 incluy yval
>       inp = (inpx,inpy) -- the actual argument to the circuit
>       (inp',intree) =
>         tuple2 "inputs" InPort (TopCluster undefined 0)
>           (bit "a") (bit "b") inp
>       out = foocirc inp'
>       (out',outtree) =
>         tuple2 "outputs" OutPort (TopCluster undefined 1)
>           (bit "x") (bit "y") out
>       (resultx,resulty) = out'
>   in do putStrLn "test13"
>         putStrLn ("inpx = " ++ show (altsem inpx))
>         putStrLn ("inpy = " ++ show (altsem inpy))
>         putStrLn ("resultx = " ++ show (altsem resultx))
>         putStrLn ("resulty = " ++ show (altsem resulty))
>         putStrLn ("Input source tree = " ++ showCluRep inclu)
>         putStrLn ("Inport tree       = " ++ showCluRep intree)
>         putStrLn ("Outport tree      = " ++ showCluRep outtree)
>         return ()


Constructing Black Box Circuits with Unclustered Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> box11Struc
>   :: Signal a
>   => String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b

> box11Struc sp laba labx tag f a =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu]
>             , boxInPorts  = [aclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       x     = f ai
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai]
>   in xout

> foobar :: a -> a -> a
> foobar x dummy = x

> box21Struc
>   :: Signal a
>   => String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box21Struc sp laba labb labx tag f a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu]
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       x     = f ai bi
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi]
>   in xout

> box22Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> (StrucSig a b, StrucSig a b)

> box22Struc sp laba labb labx laby tag f a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu]
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = outclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       (x,y) = f ai bi
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (ParentCluster outclu 0) xout
>       yout  = Alias OutPort (ParentCluster yclu 0) y
>       yclu  = Singleton (Just laby) (ParentCluster outclu 1) yout
>       outclu = Tuple2 Nothing (TopCluster box 2) xclu yclu
>       (bs,ps) = traceback [] [x,y] [] [ai,bi]
>   in (xout,yout)


> box31Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b  -> StrucSig a b
>       -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box31Struc sp laba labb labc labx tag f a b c =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu,cclu]
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       ci    = Alias InPort (ParentCluster cclu 0) c
>       cclu  = Singleton (Just labc) (TopCluster box 2) ci
>       x     = f ai bi ci
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi,ci]
>   in xout

> box32Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>     -> (StrucSig a b, StrucSig a b)

> box32Struc sp laba labb labc labx laby tag f a b c =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu,cclu]
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       ci    = Alias InPort (ParentCluster cclu 0) c
>       cclu  = Singleton (Just labc) (TopCluster box 2) ci
>       (x,y) = f ai bi ci
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       yout  = Alias OutPort (ParentCluster yclu 0) y
>       xclu  = Singleton (Just labx) (ParentCluster outclu 0) xout
>       yclu  = Singleton (Just laby) (ParentCluster outclu 1) yout
>       outclu = Tuple2 Nothing (TopCluster box 3) xclu yclu
>       (bs,ps) = traceback [] [x,y] [] [ai,bi,ci]
>   in (xout,yout)

> box61Struc
>   :: Signal a
>   => String
>   -> String -> String -> String
>   -> String -> String -> String
>   -> String
>   -> b
>   -> (StrucSig a b -> StrucSig a b  -> StrucSig a b
>   ->  StrucSig a b -> StrucSig a b  -> StrucSig a b
>       -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> StrucSig a b

> box61Struc sp labi0 labi1 labi2 labi3 labi4 labi5
>  labx tag f a0 a1 a2 a3 a4 a5 =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a0)
>             , boxChildren = bs
>             , boxPorts    = [a0clu,a1clu,a2clu,a3clu,a4clu,a5clu]
>             , boxInPorts  = [a0clu,a1clu,a2clu,a3clu,a4clu,a5clu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       a0i   = Alias InPort (ParentCluster a0clu 0) a0
>       a0clu = Singleton (Just labi0) (TopCluster box 0) a0i
>       a1i   = Alias InPort (ParentCluster a1clu 1) a1
>       a1clu = Singleton (Just labi1) (TopCluster box 1) a1i
>       a2i   = Alias InPort (ParentCluster a2clu 2) a2
>       a2clu = Singleton (Just labi2) (TopCluster box 2) a2i
>       a3i   = Alias InPort (ParentCluster a3clu 3) a3
>       a3clu = Singleton (Just labi3) (TopCluster box 3) a3i
>       a4i   = Alias InPort (ParentCluster a4clu 4) a4
>       a4clu = Singleton (Just labi4) (TopCluster box 4) a4i
>       a5i   = Alias InPort (ParentCluster a5clu 5) a5
>       a5clu = Singleton (Just labi5) (TopCluster box 5) a5i
>       x     = f a0i a1i a2i a3i a4i a5i
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 6) xout
>       (bs,ps) = traceback [] [x] [] [a0i,a1i,a2i,a3i,a4i,a5i]
>   in xout




======================================================================
SigStructSaveRecovered.lhs
____________________________________________________________________
	 The Hydra Computer Hardware Description Language
	       Structural Representation of Circuits

Copyright (c) 2001 John O'Donnell.  See the README file for general
information and documentation, COPYING for the full copyright, and
the web page for updates: http://www.dcs.gla.ac.uk/~jtod/Hydra/
____________________________________________________________________

> module SigStruct where

SigStruct defines the structural representation of signals and
black box circuits.  These can  be used to provide a variety of
services that require knowledge of the structure of a circuit as
well as its semantics, including the generation of netlists
and the traversal circuits in order to find signals or components.

> import Signal
> import HaskTools
> import SigBool  -- for testing, temp

Representation of Black Box Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Box type describes the structure of a black box circuit.  It
has fields with the following polymorphic types:

  a = Type of the alternative semantics attached to structural
      signals; for example Stream Bool.
  b = Type of a tag that can be attached to each instance of a box
      in order to hold extra information, such as an instance
      identification or a geometric layout.

> data Signal a => Box a b
>  = Box
>     { boxSpecies  :: String
>     , boxTag      :: b
>     , boxParent   :: BoxParent a b
>     , boxChildren :: [Box a b]
>     , boxPorts    :: [Cluster (StrucSig a b) (Box a b)]
>     , boxInPorts  :: [Cluster (StrucSig a b) (Box a b)]
>     , boxOutPorts :: Cluster (StrucSig a b) (Box a b)
>     , boxSignals  :: [StrucSig a b]
>     , boxWires    :: [Wire (StrucSig a b)]
>     }

The boxSpecies is a generic circuit name, indicating what kind of
circuit it is, such as "half adder" or "74x000".  This is
represented as a String, but a more structured representation for
the generic circuit type could also be used.

The boxTag can be used to attach any additional information to the
box, such as geometric layout or anything else.

The BoxParent type specifies where a black box circuit is within
the entire circuit hierarchy.  It is either a top box (which means
the box is not a child of any known box; there may be several top
boxes), or else it is a child with a parent box and an Int that
gives its sibling number; this can be used to index this box within
its parent's list of children.

> data BoxParent a b
>   = TopBox
>   | ChildOfBox (Box a b)

The boxChildren field contains a list of the black boxes used
within the box.  Only circuits mentioned explicitly in the box
definition are listed; circuits that are more deeply nested do not
appear in boxChildren, but they can be obtained by a recursive
traversal.


Structural Representation of Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A `structural signal' describes exactly where in the circuit the
signal is found, along with some additional useful information.
The type paramater a is used to attach an alternative semantic
interpretation to the structural signal.  This could be a
simulation semantics, such as a Boolean signal value or a Stream.

a is the signal semantics type, b is the box tag type.

> data StrucSig a b
>   = Definer
>       (ClusterParent (StrucSig a b) (Box a b))
>       a
>   | Alias
>       Port
>       (ClusterParent (StrucSig a b) (Box a b))
>       (StrucSig a b)
>   | Definer3   -- for new ClusterableThree
>       (Cluster (StrucSig a b) (Box a b))
>       a
>   | Alias3
>       Port
>       (Cluster (StrucSig a b) (Box a b))
>       (StrucSig a b)

Each structural signal contains an explicit representation of its
location within the circuit, consisting of a set up pointers up
through the cluster, and terminating eventually in the black box
that owns the signal.  This location is a ClusterParent object.

Aliased signals are marked according to their status within a box,
using the Port type.  A Tag is used to mark a point on feedback
loops, to prevent structure traversals from falling into infinite
recursions.

> data Port =
>   InPort | OutPort | BiPort | Local | BiLocal | Tag Int
>   deriving Show

> unalias (Alias p cp x) = x
> unalias _ = error "unalias: not an Alias"

> tag :: Int -> StrucSig a b -> StrucSig a b
> tag i x = Alias (Tag i) UnknownLocation x

> isInPort :: Signal a => StrucSig a b -> Bool
> isInPort (Alias InPort _ _) = True
> isInPort _ = False

A structural signal contains an alternative semantics (for example,
a stream).  The altsem function returns the alternative semantics
associated with a structural signal.

> altsem :: StrucSig a b -> a
> altsem (Definer _ x) = x
> altsem (Alias _ _ x) = altsem x
> altsem (Definer3 _ x) = x
> altsem (Alias3 _ _ x) = altsem x


Traversing a circuit graph
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are three groups of signal owned by a box: its input ports,
its output ports, and signals that are internal to the box (these
must be outputs produced by children of the box).  These are all
available in the boxPorts field.

> child :: Signal a => Box a b -> Int -> Box a b
> child b i = boxChildren b !! i

Every signal belongs to exactly one black box which is called the
`owner' of the signal.  From the viewpoint of a black box, every
signal that belongs to it is either an Input, and Output, or a
Internal signal.

> sigOwner :: StrucSig a b -> Box a b
> sigOwner = clupbox . sigParent

Each structural signal exists within a cluster, where it may have a
name and be associated with other signals.  The entire cluster
belongs to a specific black box, and the sigParent function returns
the box owning a signal.

> sigParent
>   :: StrucSig a b
>   -> ClusterParent (StrucSig a b) (Box a b)
> sigParent (Definer c _) = c
> sigParent (Alias _ c _) = c

Traverse a cluster all the way to the top cluster in the structure.

> rootCluster
>   :: Signal a
>   => Cluster (StrucSig a b) (Box a b)
>   -> Cluster (StrucSig a b) (Box a b)
> rootCluster c =
>   case clusterParent c of
>     ParentCluster c' i -> rootCluster c'
>     TopCluster b i     -> c

> sigCluster
>   :: Signal a
>   => StrucSig a b
>   -> Cluster (StrucSig a b) (Box a b)
> sigCluster x =
>   case sigParent x of
>     ParentCluster c' i -> rootCluster c'
>     TopCluster b i     ->
>       error "Signal is not embedded in a cluster"

> clupbox :: ClusterParent (StrucSig a b) (Box a d) -> Box a d
> clupbox (ParentCluster c i) = clupbox (clusterParent c)
> clupbox (TopCluster b i) = b
> clupbox _ = error "Cluster parent lacks a box"

> outPortSource :: StrucSig a b -> StrucSig a b
> outPortSource (Alias OutPort p x) = x
> outPortSource (Alias _ p x) = error "not an OutPort"
> outPortSource _ = error "not an Alias"

> inPortSource :: StrucSig a b -> StrucSig a b
> inPortSource (Alias InPort p x) = x
> inPortSource (Alias _ p x) = error "not an InPort"
> inPortSource _ = error "not an Alias"

> boxinputs :: Signal a => Box a b -> [StrucSig a b]
> boxinputs b =
>   map inPortSource
>     (concat (map clusigs (boxInPorts b)))


Showing Signals, Clusters and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several ways to show a signal (that is, produce a
descriptive character string).

> showcluster :: Int -> Cluster (StrucSig a b) (Box a b) -> String
> showcluster i x =
>   "port " ++ show i ++ ": " ++ showclunames x

> describe_sig :: StrucSig a b -> String
> describe_sig (Definer p x)
>   = describe_cluster_parent p
> describe_sig (Alias port p x)
>   = show port ++ " " ++ describe_cluster_parent p

> sigName :: Signal a => StrucSig a b -> String
> sigName x =
>   case sigParent x of
>     ParentCluster c i -> cluname c ++ "[" ++ show i ++ "]"
>     TopCluster b i -> "?["++ show i ++ "]"
>     UnknownLocation -> "?"

get rid of this soon?

> showboxInputs b =
>   "inputs to " ++ boxSpecies b ++ ": " ++ "??? TO DO ???"
> {-  ++ concat (map (++"*")
>       (map describe_sig_in_box (boxInPorts b))) -}

> describe_sig_in_box :: Signal a => StrucSig a b -> String
> describe_sig_in_box (Alias InPort p x) =
>   "\nsiginbox InPort " ++ describe_cluster_parent p
>   ++ "<-" ++ describe_sig x
>   ++ "(" ++ boxSpecies (sigOwner x) ++ ")"
> describe_sig_in_box (Alias OutPort p x) =
>   "\nsiginbox Local " ++ describe_cluster_parent p
>   ++ "<-" ++ describe_sig x

Use showHandle to print a box, given any of its output signals.
Use showboxfull to print the box, given that box itself, and
showboxquick to give a quick summary of the contents of the box.

> showHandle :: Signal a => StrucSig a b -> IO ()
> showHandle x = showboxfull 0 (sigOwner x)

> showboxquick :: Signal a => Int -> Box a b -> IO ()
> showboxquick i b =
>   do putStrLn (indent i ++ "Summary of " ++ boxSpecies b)
>      putStrLn (indent i ++ "  input ports:" ++
>             concat (map ((' ':) . showclunames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  output ports: "
>                  ++ showclunames (boxOutPorts b))

> showboxfull :: Signal a => Int -> Box a b -> IO ()
> showboxfull i b =
>   do putStrLn (indent i ++ "Black box " ++ boxSpecies b)
>      putStrLn (indent i ++ "  Input ports:" ++
>             concat (map ((' ':) . showclunames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  Output ports: "
>                  ++ showclunames (boxOutPorts b))
>      putStrLn (indent i ++ "  Interior signals:")
>      doall (\s -> putStrLn (indent i ++ "  " ++ describe_sig s))
>            (boxSignals b)
>      putStrLn (indent i ++ "  Interior circuits:")
>      doall (\b -> showboxfull (i+1) b) (boxChildren b)
  

Wires
~~~~~

> data Wire a = Wire

> {- data Wire a
>   = Unidir (Port a) [Port a]
>   | Bidir  [Port a] -}


Comparison of Signals and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(need updating) The boxEq function compares two boxes to determine
whether they are the same, assuming that they are siblings but
without examining their horizontal or vertical locations.  This
operation is required in order to determine the list of children
within the parent, which is needed in turn in order to work out the
horizontal locations of the children.

> boxEqInBox :: Signal a => Box a b -> Box a b -> Bool
> boxEqInBox b1 b2 =
>   let b1inps = boxinputs b1
>       b2inps = boxinputs b2
>   in boxSpecies b1 == boxSpecies b2
>      && and (zipWith sigEqInBox b1inps b2inps)

(need updating) The following function determines whether two
signals are the same.  The function assumes that they belong to the
same box but it does not use the horizontal or vertical locations.
Within a box b, two signals are the same if either they are both
(the same) input to b, or if they are both (the same) output from
the same box (which is a child of b), or if they are the same
internal signal within the box.

> sigEqInBox :: Signal a => StrucSig a b -> StrucSig a b -> Bool
> sigEqInBox (Definer _ _) (Definer _ _) =
>   error "both args are Definers"
> sigEqInBox (Definer _ _) (Alias _ _ _) =
>   error "first arg is Definer"
> sigEqInBox (Alias _ _ _) (Definer _ _) =
>   error "second arg is Definer"
> sigEqInBox (Alias z1 p1 a1) (Alias z2 p2 a2) =
>   case (z1,z2) of
>     (OutPort,OutPort) ->
>       case cmpSigLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> boxEqInBox b1 b2
>     (InPort,InPort) ->
>       case cmpSigLoc p1 p2 of
>         Nothing -> False
>         Just (b1,b2) -> True
>     (Tag t1, Tag t2) -> t1==t2
>     (InPort,OutPort) -> False
>     (OutPort,InPort) -> False
>     (_,_) -> False

> cmpSigLoc
>   :: Signal a
>   => ClusterParent (StrucSig a b) (Box a b)
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)
> cmpSigLoc (TopCluster b1 i1) (TopCluster b2 i2) =
>   if i1==i2
>   then Just (b1,b2)
>   else Nothing
> cmpSigLoc (ParentCluster c1 i1) (ParentCluster c2 i2) =
>   if i1==i2
>   then cmpCluLoc c1 c2
>   else Nothing
> cmpSigLoc _ _ = Nothing

> cmpCluLoc
>   :: Signal a
>   => Cluster (StrucSig a b) (Box a b)
>   -> Cluster (StrucSig a b) (Box a b)
>   -> Maybe (Box a b, Box a b)
> cmpCluLoc (Singleton _ p1 _) (Singleton _ p2 _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Tuple2 _ p1 _ _) (Tuple2 _ p2 _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Tuple3 _ p1 _ _ _) (Tuple3 _ p2 _ _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc (Word _ p1 _ _) (Word _ p2 _ _) =
>   cmpSigLoc p1 p2
> cmpCluLoc _ _ = Nothing


Discovering the contents of a box
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

(old description, needs updating) The traceback function takes the
list of outputs from a black box, and it delivers a list of the
children of that box.  This list, in turn, is used to define the
horizontal locations of those children: a child's horizontal
location is its index in this list.  The function builds the list
by starting with the outputs, and tracing all the signals backwards
through the box, adding each newly discovered box to the
accumulator.  The traceback does not go through the internal
structure of a child; instead, it teleports directly from the
output of a child back to its inputs.  A branch of the traceback
terminates when an input to the box is encountered.  This function
assumes that the box circuit is a directed acyclic graph; if cycles
are present, the traceback will not terminate.

The first accumulator, xs, is the list of signals that remain to be
traced back.  The second accumulator, bs, is the list of children
discovered so far.  It is essential that each child appears exactly
once in the final result of traceback.

> traceback
>   :: Signal a
>   => [Int]
>   -> [StrucSig a b]
>   -> [Box a b]
>   -> [StrucSig a b]
>   -> ([Box a b], [StrucSig a b])

> traceback seen [] bs ss = (bs,ss)
> traceback seen (x:xs) bs ss =
>   case x of
>     Alias z p a ->
>       case z of
>         InPort -> traceback seen xs bs ss
>         Tag i ->
>           if member i seen
>             then traceback seen xs bs ss
>             else traceback (i:seen) (a:xs) bs ss
>         OutPort ->
>           if or (map (sigEqInBox x) ss)
>             then traceback seen xs bs ss
>             else let b = sigOwner x
>                  in if or (map (boxEqInBox b) bs)
>                       then traceback seen xs bs (ss++[x])
>                       else traceback seen (boxinputs b ++ xs)
>                              (bs++[b]) (ss++[x])
>         _ -> error "traceback: funny port, not In/Out port"
>     _ ->  error "Badly formed signal, not an alias"


Constructing Black Boxes for Primitive Components
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The prim01Struc function builds a structural representation of a
primitive component that takes no inputs, and produces a singleton
output signal.  This is suitable for defining constant signals and
top level circuit inputs.  Its arguments are:

  x   :: a  = the alternative semantics of the input signal.
  l   :: b  = the label for this input signal.
  sp  :: c  = the species of box (e.g. "Input")
  tag :: d  = tag decorating the box

> prim01Struc
>   :: (Signal a)
>   => a -> String -> String -> d
>   -> StrucSig a d

> prim01Struc x l sp tag =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = []
>             , boxOutPorts = yclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       y    = Definer (ParentCluster yclu 0) x
>       yout = Alias OutPort (ParentCluster yclu 0) y
>       yclu = Singleton (Just l) (TopCluster box 0) yout
>   in yout

The prim11 function builds a primitive black box circuit that takes
one input and produces one output.  The arguments have the
following meanings: f computes the semantics of the output; laba is
the label for the input signal a, labx is the label for the output
signal x, sp is the box species and tag allows arbitrary
information to be attached to the circuit.

> prim11Struc
>   :: (Signal a)
>   => (a->a) -> String -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d

> prim11Struc f laba labx sp tag a =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       x    = Definer (ParentCluster xclu 0)
>                (f (altsem a))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 1) xout
>   in xout

A primitive that takes j singleton inputs and produces k singleton
outputs can be defined by a function whose name has the form
primjkStruc.  These functions have definitions similar to that of
prim11Struc above.

> prim21Struc
>   :: (Signal a)
>   => (a->a->a) -> [String] -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d -> StrucSig a d

> prim21Struc f [laba,labb] labx sp tag a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       bi   = Alias InPort (ParentCluster bclu 0) b
>       bclu = Singleton (Just labb) (TopCluster box 1) bi
>       x    = Definer (ParentCluster xclu 0)
>               (f (altsem a) (altsem b))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 2) xout
>   in xout

> prim31Struc
>   :: (Signal a)
>   => (a->a->a->a) -> [String] -> String -> String -> d
>   -> StrucSig a d -> StrucSig a d -> StrucSig a d -> StrucSig a d

> prim31Struc f [laba,labb,labc] labx sp tag a b c =
>   let box =
>         Box {boxSpecies   = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxPorts    = []
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = []
>             , boxWires    = []
>             }
>       ai   = Alias InPort (ParentCluster aclu 0) a
>       aclu = Singleton (Just laba) (TopCluster box 0) ai
>       bi   = Alias InPort (ParentCluster bclu 0) b
>       bclu = Singleton (Just labb) (TopCluster box 1) bi
>       ci   = Alias InPort (ParentCluster bclu 0) c
>       cclu = Singleton (Just labc) (TopCluster box 2) ci
>       x    = Definer (ParentCluster xclu 0)
>               (f (altsem a) (altsem b) (altsem c))
>       xout = Alias OutPort (ParentCluster xclu 0) x
>       xclu = Singleton (Just labx) (TopCluster box 3) xout
>   in xout


Standard Primitive Circuit Structures
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Constant signals

> conStruc :: Signal a => String -> a -> StrucSig a b
> conStruc l x = prim01Struc x l "Constant" undefined

Inputs to a circuit

> input :: Signal a => String -> a -> StrucSig a b
> input l x = prim01Struc x l "Input" undefined

The buffer and inverter

> bufStruc, invStruc
>   :: Signal a
>   => StrucSig a d -> StrucSig a d
> bufStruc = prim11Struc buf "a" "x" "buf" undefined
> invStruc = prim11Struc inv "a" "x" "inv" undefined

> dffStruc
>   :: (Signal a, Clocked a)
>   => StrucSig a b -> StrucSig a b
> dffStruc = prim11Struc dff "dff_ld" "dff_a" "dff" undefined

Basic logic gates

> and2Struc, or2Struc, xor2Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b
> and2Struc = prim21Struc and2
>               ["and2_a","and2b"] "and2_x" "and2" undefined
> or2Struc  = prim21Struc or2
>               ["or2_a","or2_b"] "or2_x" "or2"  undefined
> xor2Struc = prim21Struc xor2
>               ["xor2_a","xor2_b"] "xor2_x" "xor2" undefined

> and3Struc, or3Struc
>   :: Signal a
>   => StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b
> and3Struc = prim31Struc and3
>               ["and3_a","and3_b","and3_c"] "and3_x" "and3"
>               undefined
> or3Struc  = prim31Struc or3
>               ["or3_a","or3_b","or3_c"] "or3_x" "or3"  undefined


Signal Instances for Standard Structural Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> instance Signal a => Signal (StrucSig a d)
>   where
>     sglsig = undefined  -- ??? need this?
>     is0  = is0 . altsem
>     is1  = is1 . altsem
>     zero = conStruc "0" zero
>     one  = conStruc "1" one
>     buf  = bufStruc
>     inv  = invStruc
>     and2 = and2Struc
>     or2  = or2Struc
>     xor2 = xor2Struc
>     and3 = and3Struc
>     or3 = or3Struc
>     box11 = undefined --box11Struc
>     box21 = undefined -- box21Struc
>     box22 = undefined --box22Struc
>     box31 = undefined --box31Struc
>     box41 = undefined
> --     mkOut = undefined --mkOutStruct

> instance (Signal a, Clocked a) => Clocked (StrucSig a b) where
>   dff = dffStruc



Tuple/Cluster Conversion
~~~~~~~~~~~~~~~~~~~~~~~~

The LabClu class is used to convert a cluster of strings from
Haskell tuple types to the explicit Cluster data structure, which
can then be used to attach the labels to signals in the Clusterable
class.

> class LabClu a c where
>   labClu :: a -> Cluster String c

> instance LabClu String c where
>   labClu s = Singleton (Just s) undefined undefined

> instance (LabClu a c, LabClu b c) => LabClu (a,b) c  where
>   labClu (x,y) = Tuple2 Nothing undefined (labClu x) (labClu y)

> instance (LabClu a d, LabClu b d, LabClu c d)
>   => LabClu (a,b,c) d where
>   labClu (x,y,z) =
>     Tuple3 Nothing undefined (labClu x) (labClu y) (labClu z)

Conversion between nested tuples and lists and the explicit cluster
representation.  The underlying signal type is a, the actual cluster
type is b, and the structural tag type is c.  For example, we could
have a = Bool, b = (Bool,(Bool,Bool)) and c = ().


The barbaz function can be used to force the two clusters to be
constructed from the same underlying signal and box tag types.

> barbaz :: Cluster a b -> Cluster a b -> ()
> barbaz x y = ()

try a simpler clustering mechanism there is no conversion from a
Cluster representation to a cluster; all conversions go from the
real cluster (nested tuples) to the cluster rep.  The mkAlias
function builds a new cluster that is isomorphic to the old one,
with InPort/OutPort alias in place.  The mkCluRep operation builds
the cluster representation tree which can be used to find out where
signals are.

> class ClusterableTwo a where
>   mkAlias :: Port -> a -> a

> instance ClusterableTwo (StrucSig a b) where
>   mkAlias port x = Alias port undefined x

> instance (ClusterableTwo a, ClusterableTwo b)
>  => ClusterableTwo (a,b) where
>  mkAlias port (x,y) = (mkAlias port x, mkAlias port y)

> putLabel
>   :: ClusterParent a c
>   -> Cluster String c
>   -> Cluster a c
>   -> Cluster a c

> putLabel cp (Singleton _ _ s) (Singleton _ _ x) =
>   Singleton (Just s) cp x

> putLabel cp (Tuple2 _ _ a b) (Tuple2 _ _ x y) =
>   let r = Tuple2 Nothing cp
>             (putLabel (ParentCluster r 0) a x)
>             (putLabel (ParentCluster r 1) b y)
>   in r

> showCluRep :: Show a => Cluster (StrucSig a b) c -> String
> showCluRep (Singleton s _ x) = strucSigStruc x
> showCluRep (Tuple2 s p a b) =
>   "(" ++ showCluRep a ++ ", " ++ showCluRep b ++ ")"
> showCluRep _ = error "bad case"

> strucSigStruc :: Show a => StrucSig a b -> String
> strucSigStruc (Definer _ x) = "Definer-" ++ show x
> strucSigStruc (Definer3 _ x) = "Definer-" ++ show x
> strucSigStruc (Alias p _ x) =
>    "Alias-" ++ show p ++ " " ++ strucSigStruc x
> strucSigStruc (Alias3 p _ x) =
>    "Alias3-" ++ show p ++ " " ++ strucSigStruc x

> class CluFlat a b c where
>   cluit :: a -> Cluster b c

> instance CluFlat (StrucSig a b) (StrucSig a b) c where
>   cluit x = Singleton undefined undefined x

> instance (CluFlat a t c, CluFlat b t c) => CluFlat (a,b) t c where
>   cluit (x,y) = Tuple2 Nothing undefined (cluit x) (cluit y)


> testsigA   = Definer UnknownLocation True  :: StrucSig Bool ()
> testsigB   = Definer UnknownLocation False :: StrucSig Bool ()
> testpairAB = (testsigA, testsigB)

> test1 =
>   do putStrLn "Test mkAlias method (of class ClusterableTwo) on singleton"
>      let x = mkAlias InPort testsigA
>      let y = mkAlias OutPort testsigA
>      putStrLn (strucSigStruc testsigA)
>      putStrLn (strucSigStruc x)
>      putStrLn (strucSigStruc y)

> test7 =
>   do putStrLn "asdf"
>      let a = cluit testsigA :: Cluster (StrucSig Bool ()) ()
>      putStrLn (showCluRep a)
>      let b = cluit (testsigA,testsigB) :: Cluster (StrucSig Bool ()) ()
>      putStrLn (showCluRep b)
>      return ()

> bboxtest1 alab xlab f a =
>   do let alabclu = labClu alab
>      let a' = mkAlias InPort a
>      let a'rep = cluit a'  :: Cluster (StrucSig Bool ()) ()
>      let a'' = putLabel UnknownLocation alabclu a'rep
>                   :: Cluster (StrucSig Bool ()) ()
>      putStrLn ("a'' = " ++ showCluRep a'')
>      let x = f a'
>      let x' = mkAlias OutPort x
>      let xlabclu = labClu xlab
>      let x'rep = cluit x' :: Cluster (StrucSig Bool ()) ()
>      let x'' = putLabel UnknownLocation xlabclu x'rep
>      putStrLn ("x'' = " ++ showCluRep x'')
>      let clus = [a'',x'']
>      putStrLn "bboxtest1"
>      return ()

> test8 = bboxtest1 "abc" "def" inv testsigA
> test9 = bboxtest1 ("ab","cd") ("ef","gh") ha (testsigA,testsigB)

> ha (x,y) = (and2 x y, or2 x y)

q is the tuple type, a is the signal semantics type, b is the box
tag type.

> class ClusterableThree a b q where
>   mkAlias3
>     :: Port
>     -> LabTree
>     -> ClusterParent (StrucSig a b) (Box a b)
>     -> q
>     -> (q, Cluster (StrucSig a b) (Box a b))

> instance ClusterableThree a b (StrucSig a b) where
>   mkAlias3 port label cp x =
>     let x' = Alias3 port c x
>         c = Singleton lab cp x'
>         lab = case label of
>                 LabTree1 s -> s
>                 _ -> Just "bad singleton label"
>     in (x',c)

> instance (ClusterableThree a b x, ClusterableThree a b y)
>   => ClusterableThree a b (x,y) where
>   mkAlias3 port label cp (x,y) =
>     let (x',cx) = mkAlias3 port labx (ParentCluster p 0) x
>         (y',cy) = mkAlias3 port laby (ParentCluster p 1) y
>         p  = Tuple2 lab cp cx cy
>         (lab,labx,laby) =
>           case label of
>             LabTree2 lab labx laby -> (lab,labx,laby)
>             _ -> (Just "bad pair label 1",
>                   LabTree1 (Just "bad pair label 2"),
>                   LabTree1 (Just "bad pair label 3"))
>     in ((x',y'), p)

> class ClusterableFour a where
>   mkalias4
>     :: Port
>     -> LabTree
>     -> ClusterParent (StrucSig Bool ()) (Box Bool ())
>     -> a
>     -> (a, Cluster (StrucSig Bool ()) (Box Bool ()))

> instance ClusterableFour (StrucSig Bool ()) where
>   mkalias4 port label cp x =
>     let x' = Alias3 port c x
>         c = Singleton lab cp x'
>         lab = case label of
>                 LabTree1 s -> s
>                 _ -> Just "bad singleton label"
>     in (x',c)
  
> data LabTree
>   = LabTree1 (Maybe String)
>   | LabTree2 (Maybe String) LabTree LabTree
>   | LabTree3 (Maybe String) LabTree LabTree LabTree

> {- bboxtest2
>   :: (ClusterableThree a b p, ClusterableThree a b q)
>   => LabTree
>   -> LabTree
>   -> (p->q)
>   -> p -> q -}

> bboxtest2 alab xlab f a =
>   do let (a',atree) = mkAlias3 InPort alab UnknownLocation a
>      let x = f a'
>      let dummy1 = foobar a a'
>      let dummy2 = foobar x a'
>      let (x',xtree) = mkAlias3 OutPort xlab UnknownLocation x
>  --    putStrLn (showCluRep atree)
>  --    putStrLn (showCluRep xtree)
>      let clus = [atree,xtree]
>                  :: [Cluster (StrucSig Bool ()) (Box Bool ())]
>      putStrLn (show (altsem a))
>      putStrLn (show (altsem x))
>      return ()

> test10 =
>   bboxtest2
>     (LabTree1 (Just "a"))
>     (LabTree1 (Just "x"))
>     (inv :: StrucSig Bool () -> StrucSig Bool ())
>     testsigA


> bboxtest3 mkalias alab xlab f a =
>   do putStr "bboxtest3"
>      let (a',atree) = mkalias InPort alab UnknownLocation a
>      putStrLn ("input cluster = " ++ showCluRep atree)
>      let x = f a'
>      let (x',xtree) = mkalias OutPort xlab UnknownLocation x
>      return ()

> test11 =
>   bboxtest3
>     mkalias4
>     (LabTree1 (Just "a"))
>     (LabTree1 (Just "x"))
>     (inv :: StrucSig Bool () -> StrucSig Bool ())
>     testsigA

New Approach (May 14, 2001)
~~~~~~~~~~~~~~~~~~~~~~~~~~~

> bit
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> StrucSig a b
>   -> (StrucSig a b, Cluster (StrucSig a b) (Box a b))

> bit label port parent x =
>   let x' = Alias3 port clutree x
>       clutree = Singleton (Just label) parent x'
>   in (x', clutree)

> tuple2
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, Cluster (StrucSig a b) (Box a b)))
>   -> (c,d)
>   -> ((c,d), Cluster (StrucSig a b) (Box a b))

> tuple2 label port parent f g (x,y) =
>   let tree = Tuple2 (Just label) parent xtree ytree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>   in ((x',y'), tree)

> tuple3
>   :: String
>   -> Port
>   -> ClusterParent (StrucSig a b) (Box a b)
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> c -> (c, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> d -> (d, Cluster (StrucSig a b) (Box a b)))
>   -> (Port -> ClusterParent (StrucSig a b) (Box a b)
>        -> e -> (e, Cluster (StrucSig a b) (Box a b)))
>   -> (c,d,e)
>   -> ((c,d,e), Cluster (StrucSig a b) (Box a b))

> tuple3 label port parent f g h (x,y,z) =
>   let tree = Tuple3 (Just label) parent xtree ytree ztree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>       (z',ztree) = h port (ParentCluster tree 1) z
>   in ((x',y',z'), tree)


A circuit with singleton input and output clusters

> test12 :: Bool -> IO ()
> test12 xval =
>   let x = Definer3 inpclu xval
>       inpclu = Singleton (Just "input") (TopCluster undefined 0) x
>       (x',xt) = bit "x" InPort (TopCluster undefined 0) x
>       y = inv x
>       (y',yt) = bit "y" OutPort (TopCluster undefined 1) y
>   in do putStrLn "test12"
>         putStrLn ("x  = " ++ show (altsem x))
>         putStrLn ("x' = " ++ show (altsem x'))
>         putStrLn ("y  = " ++ show (altsem y))
>         putStrLn ("y' = " ++ show (altsem y'))
>         putStrLn ("Input source tree = " ++ showCluRep inpclu)
>         putStrLn ("Inport tree       = " ++ showCluRep xt)
>         putStrLn ("Outport tree      = " ++ showCluRep yt)
>         return ()

A circuit with a pair input and pair output

> foocirc (x,y) = (and2 x y, or2 x y)

> test13 :: (Bool,Bool) -> IO ()
> test13 (xval,yval) =
>   let inclu = Tuple2 Nothing (TopCluster undefined 0)
>                 inclux incluy
>       inclux = Singleton (Just "inx") (ParentCluster inclu 0) inpx
>       incluy = Singleton (Just "iny") (ParentCluster inclu 1) inpy
>       inpx = Definer3 inclux xval
>       inpy = Definer3 incluy yval
>       inp = (inpx,inpy) -- the actual argument to the circuit
>       (inp',intree) =
>         tuple2 "inputs" InPort (TopCluster undefined 0)
>           (bit "a") (bit "b") inp
>       out = foocirc inp'
>       (out',outtree) =
>         tuple2 "outputs" OutPort (TopCluster undefined 1)
>           (bit "x") (bit "y") out
>       (resultx,resulty) = out'
>   in do putStrLn "test13"
>         putStrLn ("inpx = " ++ show (altsem inpx))
>         putStrLn ("inpy = " ++ show (altsem inpy))
>         putStrLn ("resultx = " ++ show (altsem resultx))
>         putStrLn ("resulty = " ++ show (altsem resulty))
>         putStrLn ("Input source tree = " ++ showCluRep inclu)
>         putStrLn ("Inport tree       = " ++ showCluRep intree)
>         putStrLn ("Outport tree      = " ++ showCluRep outtree)
>         return ()


Constructing Black Box Circuits with Unclustered Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> box11Struc
>   :: Signal a
>   => String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b

> box11Struc sp laba labx tag f a =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu]
>             , boxInPorts  = [aclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       x     = f ai
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai]
>   in xout

> foobar :: a -> a -> a
> foobar x dummy = x

> box21Struc
>   :: Signal a
>   => String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box21Struc sp laba labb labx tag f a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu]
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       x     = f ai bi
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi]
>   in xout

> box22Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> (StrucSig a b, StrucSig a b)

> box22Struc sp laba labb labx laby tag f a b =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu]
>             , boxInPorts  = [aclu,bclu]
>             , boxOutPorts = outclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       (x,y) = f ai bi
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (ParentCluster outclu 0) xout
>       yout  = Alias OutPort (ParentCluster yclu 0) y
>       yclu  = Singleton (Just laby) (ParentCluster outclu 1) yout
>       outclu = Tuple2 Nothing (TopCluster box 2) xclu yclu
>       (bs,ps) = traceback [] [x,y] [] [ai,bi]
>   in (xout,yout)


> box31Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b  -> StrucSig a b
>       -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b -> StrucSig a b

> box31Struc sp laba labb labc labx tag f a b c =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu,cclu]
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       ci    = Alias InPort (ParentCluster cclu 0) c
>       cclu  = Singleton (Just labc) (TopCluster box 2) ci
>       x     = f ai bi ci
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 2) xout
>       (bs,ps) = traceback [] [x] [] [ai,bi,ci]
>   in xout

> box32Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> String -> b
>   -> (StrucSig a b -> StrucSig a b -> StrucSig a b
>       -> (StrucSig a b, StrucSig a b))
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>     -> (StrucSig a b, StrucSig a b)

> box32Struc sp laba labb labc labx laby tag f a b c =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxPorts    = [aclu,bclu,cclu]
>             , boxInPorts  = [aclu,bclu,cclu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       ai    = Alias InPort (ParentCluster aclu 0) a
>       aclu  = Singleton (Just laba) (TopCluster box 0) ai
>       bi    = Alias InPort (ParentCluster bclu 0) b
>       bclu  = Singleton (Just labb) (TopCluster box 1) bi
>       ci    = Alias InPort (ParentCluster cclu 0) c
>       cclu  = Singleton (Just labc) (TopCluster box 2) ci
>       (x,y) = f ai bi ci
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       yout  = Alias OutPort (ParentCluster yclu 0) y
>       xclu  = Singleton (Just labx) (ParentCluster outclu 0) xout
>       yclu  = Singleton (Just laby) (ParentCluster outclu 1) yout
>       outclu = Tuple2 Nothing (TopCluster box 3) xclu yclu
>       (bs,ps) = traceback [] [x,y] [] [ai,bi,ci]
>   in (xout,yout)

> box61Struc
>   :: Signal a
>   => String
>   -> String -> String -> String
>   -> String -> String -> String
>   -> String
>   -> b
>   -> (StrucSig a b -> StrucSig a b  -> StrucSig a b
>   ->  StrucSig a b -> StrucSig a b  -> StrucSig a b
>       -> StrucSig a b)
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> StrucSig a b -> StrucSig a b -> StrucSig a b
>   -> StrucSig a b

> box61Struc sp labi0 labi1 labi2 labi3 labi4 labi5
>  labx tag f a0 a1 a2 a3 a4 a5 =
>   let box =
>         Box { boxSpecies  = sp
>             , boxTag      = tag
>             , boxParent   = ChildOfBox (sigOwner a0)
>             , boxChildren = bs
>             , boxPorts    = [a0clu,a1clu,a2clu,a3clu,a4clu,a5clu]
>             , boxInPorts  = [a0clu,a1clu,a2clu,a3clu,a4clu,a5clu]
>             , boxOutPorts = xclu
>             , boxSignals  = ps
>             , boxWires    = []
>             }
>       a0i   = Alias InPort (ParentCluster a0clu 0) a0
>       a0clu = Singleton (Just labi0) (TopCluster box 0) a0i
>       a1i   = Alias InPort (ParentCluster a1clu 1) a1
>       a1clu = Singleton (Just labi1) (TopCluster box 1) a1i
>       a2i   = Alias InPort (ParentCluster a2clu 2) a2
>       a2clu = Singleton (Just labi2) (TopCluster box 2) a2i
>       a3i   = Alias InPort (ParentCluster a3clu 3) a3
>       a3clu = Singleton (Just labi3) (TopCluster box 3) a3i
>       a4i   = Alias InPort (ParentCluster a4clu 4) a4
>       a4clu = Singleton (Just labi4) (TopCluster box 4) a4i
>       a5i   = Alias InPort (ParentCluster a5clu 5) a5
>       a5clu = Singleton (Just labi5) (TopCluster box 5) a5i
>       x     = f a0i a1i a2i a3i a4i a5i
>       xout  = Alias OutPort (ParentCluster xclu 0) x
>       xclu  = Singleton (Just labx) (TopCluster box 6) xout
>       (bs,ps) = traceback [] [x] [] [a0i,a1i,a2i,a3i,a4i,a5i]
>   in xout



======================================================================
SignalSave.lhs
____________________________________________________________________
	 The Hydra Computer Hardware Description Language
			      Signals

Copyright (c) 2001 John O'Donnell.  See the README file for general
information and documentation, COPYING for the full copyright, and
the web page for updates: http://www.dcs.gla.ac.uk/~jtod/Hydra/
___________________________________________________________________

> module Signal where


The Signal Class
~~~~~~~~~~~~~~~~

> class Signal a where
>   sglsig :: a -> Cluster a ()
>   zero, one :: a
>   is0, is1  :: a -> Bool
>   buf, inv :: a -> a
>   and2, or2, xor2 :: a -> a -> a
>   and3, or3 :: a -> a -> a -> a
>   box11 :: String -> String -> String
>            -> (a->a) -> a-> a
>   box21 :: String -> String -> String -> String
>            -> (a->a->a) -> a -> a -> a
>   box31 :: String -> String -> String -> String -> String
>            -> (a->a->a->a) -> a -> a -> a -> a
>   box41 :: String -> String -> String -> String
>            -> String -> String
>            -> (a->a->a->a->a) -> a -> a -> a -> a -> a
>   box22 :: String -> String -> String -> String
>            -> (String,String)
>            -> (a->a->(a,a)) -> a -> a -> (a,a)
> --   mkOut :: String -> [CluSpec a] -> CluSpec a -> Cluster a


Lattice Signals
~~~~~~~~~~~~~~~

> class Lattice a where
>   defined, shorted :: a -> Bool
>   weak, strong  :: a -> Bool
>   lub :: a -> a -> a


Clustered Signals
~~~~~~~~~~~~~~~~~

a is the signal type, b is the box type.  For example, if a box
uses a tag type t, then the box type would be Box a t and a signal
cluster would have type Cluster a (Box a t).  If the signal is
structured, then its type would be StrucSig a t and we would have a
cluster of type Cluster (StrucSig a t) (Box a t).

> data Cluster a b
>   = Singleton (Maybe String) (ClusterParent a b)
>       a
>   | Tuple2 (Maybe String) (ClusterParent a b)
>       (Cluster a b) (Cluster a b)
>   | Tuple3 (Maybe String) (ClusterParent a b)
>       (Cluster a b) (Cluster a b) (Cluster a b)
>   | Word (Maybe String) (ClusterParent a b)
>       Int [Cluster a b]


> getSgl :: Cluster a b -> a
> getSgl (Singleton _ _ x) = x
> getSgl _ = error "not a singleton"

> clusigs :: Cluster a b -> [a]
> clusigs (Singleton _ _ x) = [x]
> clusigs (Tuple2 _ _ x y) = clusigs x ++ clusigs y
> clusigs (Tuple3 _ _ x y z) = clusigs x ++ clusigs y ++ clusigs z
> clusigs (Word _ _ _ xs) = concat (map clusigs xs)


> {- clusterSigs :: Cluster a b -> [a]     Redundant!
> clusterSigs (Singleton _ _ x)
>   = [x]
> clusterSigs (Tuple2 _ _ a b)
>   = clusterSigs a ++ clusterSigs b
> clusterSigs (Tuple3 _ _ a b c)
>   = clusterSigs a ++ clusterSigs b ++ clusterSigs c
> clusterSigs (Word _ _ _ xs)
>   = concat (map clusterSigs xs) -}


> cluname :: Cluster a b -> String
> cluname (Singleton s _ x) = maybeName s
> cluname (Tuple2 s p a b) = maybeName s
> cluname (Tuple3 s p a b c) = maybeName s
> cluname (Word s p i xs) = maybeName s

> showclunames :: Cluster a b -> String
> showclunames (Singleton s _ x) = maybeName s
> showclunames (Tuple2 s p a b)
>   = maybeName s ++ "=(" ++ showclunames a
>     ++ "," ++ showclunames b ++ ")"
> showclunames (Tuple3 s p a b c)
>   = maybeName s ++ "=(" ++ showclunames a
>     ++ "," ++ showclunames b
>     ++ "," ++ showclunames c ++ ")"
> showclunames (Word s p i xs)
>   = maybeName s ++ "[" ++ show i ++ "]"

f shows the alternative semantics of a signal

> showclu
>   :: (a->String)
>   -> Cluster a b
>   -> String

> showclu f (Singleton s _ x) = maybeName s ++ "=" ++ f x
> showclu f (Tuple2 s p a b)
>   = maybeName s ++ "="
>     ++ "(" ++ showclu f a ++ "," ++ showclu f b ++ ")"
> showclu f (Tuple3 s p a b c) = maybeName s
> showclu f (Word s p i xs) = maybeName s


> maybeName :: Maybe String -> String
> maybeName Nothing = "_"
> maybeName (Just s) = s




Cluster Parents
~~~~~~~~~~~~~~~

> data ClusterParent a b
>   = ParentCluster (Cluster a b) Int
>   | TopCluster b Int
>   | UnknownLocation

> clusterParent :: Cluster a b -> ClusterParent a b
> clusterParent (Singleton _ p _) = p
> clusterParent (Tuple2 _ p _ _) = p
> clusterParent (Tuple3 _ p _ _ _) = p
> clusterParent (Word _ p _ _) = p


> describe_cluster_parent :: ClusterParent a b -> String
> describe_cluster_parent (ParentCluster clu i)
>   = cluname clu
> describe_cluster_parent (TopCluster box i)
>   = "boxport" ++ show i
> describe_cluster_parent UnknownLocation = "??"

> data CluSpec a
>   = S String a
>   | T2 String (CluSpec a) (CluSpec a)
>   | W String [CluSpec a]

> {-
> class Representable a where
>   toCluster :: a -> Cluster a
>   fromCluster :: Cluster a -> a
>
> instance Signal a => Representable a where
>   toCluster x = Singleton Nothing TopCluster x
>   fromCluster (Singleton _ _ x) = x
>
> instance (Representable a, Representable b)
>     => Representable (a,b) where
>   toCluster (x,y) = Tuple2 Nothing TopCluster
>                       (toCluster x) (toCluster y)
>   fromCluster (Tuple2 _ _ x y) = (fromCluster x, fromCluster y)
> -}


> mkCluster :: ClusterParent a b -> CluSpec a -> Cluster a b
> mkCluster p (S cs x) = Singleton (Just cs) p x
> mkCluster p (T2 cs x y) =
>   let c = Tuple2 (Just cs) p
>             (mkCluster (ParentCluster c 0) x)
>             (mkCluster (ParentCluster c 1) y)
>   in c
> mkCluster p (W cs xs) =
>   let c = Word (Just cs) p k (zipWith f xs [0..])
>       k = length xs
>       f x i = mkCluster (ParentCluster c i) x
>   in c



Operations on Clusters of Structural Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

operations on clusters, are these needed yet?

> {- bad, need to follow down and then up from a signal
> cpOwner :: ClusterParent ab -> b
> cpOwner (ParentCluster c _) = cpOwner (clusterParent c)
> cpOwner (ParentBox b _ _) = b -}

The following may not be useful...

>  {-
> clusterOwner :: Cluster a b c d -> Box a b c d
> clusterOwner c =
>   case clusterParent c of
>     ParentCluster p _ -> clusterOwner p
> -}




Clocked Signals
~~~~~~~~~~~~~~~

> class Clocked a where
>   dff :: a -> a

> class Unclocked a where
>  boolSig :: Bool -> a
>  intbit :: Int -> a
>  bitint :: a -> Int
>  showSig :: a -> String


The Trivial Signal ()
~~~~~~~~~~~~~~~~~~~~~

The trivial signal representation is (), which carries no
information at all.  This can be used where a signal is required
but you don't really want to specify one.  For example, the
structural signal type has a slot for an alternative signal
semantics.  If you're just interested in the graph structure but
not the alternative semantics, just use the trivial signal type, as
in StrucSig ().


> {- instance Signal () where
>   buf  _   = ()
>   inv  _   = ()
>   and2 _ _ = ()
>   or2  _ _ = ()
>   xor2 _ _ = ()
> -}



======================================================================
SigHorzNet.lhs
----------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
	      Behavioral Signals with Horizontal Netlist

Copyright (c) 2001 John O'Donnell.  See the README file for general
information, COPYING for the full copyright, and the web page for the
latest version: http://www.dcs.gla.ac.uk/~jtod/Hydra/
----------------------------------------------------------------------

> module SigHorzNet where
> import Signal


Horizontal Net Representation of Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data BH a
>   = BH a [BH a]
>   | BHprim11 String a (BH a)
>   | BHprim21 String a (BH a) (BH a)
>   | BHprim31 String a (BH a) (BH a) (BH a)
>   | BHout String String (BH a)
>   | BHin String String (BH a)
>   | BHinport String a
>   | BHconst String a

The beh function returns the behavior component of a BH signal.

> beh :: BH a -> a
> beh (BH x _) = x
> beh (BHprim11 _ x _) = x
> beh (BHprim21 _ x _ _) = x
> beh (BHout _ _ x) = beh x
> beh (BHin _ _ x) = beh x
> beh (BHinport _ x) = x
> beh (BHconst _ x) = x


Constructing BH signals
~~~~~~~~~~~~~~~~~~~~~~~

> prim11BH
>   :: Signal a
>   => String
>   -> (a->a)
>   -> BH a -> BH a
> prim11BH lab f a = BHprim11 lab (f (beh a)) a

> prim21BH
>   :: Signal a
>   => String
>   -> (a->a->a)
>   -> BH a -> BH a -> BH a
> prim21BH lab f a b = BHprim21 lab (f (beh a) (beh b)) a b

> prim31BH
>   :: Signal a
>   => String
>   -> (a->a->a->a)
>   -> BH a -> BH a -> BH a -> BH a
> prim31BH lab f a b c = BHprim31 lab (f (beh a) (beh b) (beh c)) a b c

> box11BH
>   :: Signal a
>   => String -> String -> String -> Layout
>   -> (BH a -> BH a)
>   -> BH a -> BH a

> box11BH boxlab alab xlab layout f a =
>   let a' = BHin boxlab alab a
>       x = f a'
>       x' = BHout boxlab xlab x
>   in x'


Primitive components
~~~~~~~~~~~~~~~~~~~~

> instance Signal a => Signal (BH a) where
>   zero = BHconst "zero" zero
>   one  = BHconst "one" one
>   buf  = prim11BH "buf" buf
>   inv  = prim11BH "inv" inv
>   and2 = prim21BH "and2" and2
>   or2  = prim21BH "or2" or2
>   xor2 = prim21BH "xor2" xor2
>   and3 = prim31BH "and3" and3
>   or3  = prim31BH "or3" or3

> instance Signal a => Structured (BH a) where
>   box11 = box11BH
>   box21 = undefined
>   box31 = undefined
>   box41 = undefined
>   box22 = undefined



..................................................
Junk...

> {-

> -}


======================================================================


