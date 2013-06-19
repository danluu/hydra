---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Signal where

---------------------------------------------------------------------------
			   The Signal Class
---------------------------------------------------------------------------

General Signals
~~~~~~~~~~~~~~~

> class Signal a where
>   zero, one :: a
>   buf, inv :: a -> a
>   and2, nand2, or2, nor2, xor2, xnor2 :: a -> a -> a
>   and3, nand3, or3, nor3, xor3, xnor3 :: a -> a -> a -> a
>   and4, nand4, or4, nor4, xor4, xnor4 :: a -> a -> a -> a -> a
>   alias :: a -> String -> Box a -> ClusterLoc -> a


Static Signals
~~~~~~~~~~~~~~

> class Signal a => Static a where
>   is0, is1 :: a -> Bool
>   boolSig :: Bool -> a
>   sigBool :: Bool -> a
>   intSig  :: Int -> a
>   sigInt  :: a -> Int
>   showSig :: a -> String
>   showSigChar :: a -> Char
>   readSigChar :: Char -> a

Clocked Signals
~~~~~~~~~~~~~~~

> class Signal a => Clocked a where
>   dff :: a -> a

Lattice Signals
~~~~~~~~~~~~~~~

> class Lattice a where
>   defined, shorted :: a -> Bool
>   weak, strong  :: a -> Bool
>   lub :: a -> a -> a

---------------------------------------------------------------------------
			  Signal Structures
---------------------------------------------------------------------------

> class Signal a => Structured a where
>   box11 :: String -> String -> String -> Layout
>            -> (a->a) -> a -> a
>   box21 :: String -> String -> String -> String -> Layout
>            -> (a->a->a) -> a -> a -> a
>   box31 :: String -> String -> String -> String -> String -> Layout
>            -> (a->a->a->a) -> a -> a -> a -> a
>   box41 :: String -> String -> String -> String
>            -> String -> String -> Layout
>            -> (a->a->a->a->a) -> a -> a -> a -> a -> a
>   box22 :: String -> String -> String -> String
>            -> (String,String) -> Layout
>            -> (a->a->(a,a)) -> a -> a -> (a,a)

> type Layout = ()

> class NoStruc a where
>  dummyNoStruc :: a -> ()

> instance NoStruc Bool
>   where dummyNoStruc x = ()

> instance NoStruc a => NoStruc [a]
>   where dummyNoStruc x = ()

> class Struc a where
>   dummyStruc :: a -> ()
  
A data structure that describes where a name occurs within a cluster.

> data ClusterLoc
>   = InportArg Int ClusterLoc
>   | Outport ClusterLoc   -- Int is dummy, don't need it
>   | Equation Int ClusterLoc
>   | CluVar String
>   | CluTup Int Int ClusterLoc
>   deriving Show

Representation of Black Box Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Box type describes the structure of a black box circuit with
signals of type StrucSig a, where a is the basic signal type.

> data Signal a => Box a
>   = Box
>      { boxLbl       :: String
>      , boxInPorts   :: [a]
>      , boxOutPorts  :: [a]
>      , boxLocalSigs :: [a]
>      , boxParent    :: BoxParent a
>      , boxChildren  :: [Box a]
>      }

The boxParent specifies where a circuit lies within the entire circuit
hierarchy.  It is either a top box (which means the box is not a child of
any known box; there may be several top boxes), or else it is a child
within a parent box, with an integer index giving its position within the
boxChildren list of the parent.  parent's list of children.

> data BoxParent a
>   = TopBox
>   | ChildBox (Box a) Int

Representation of structural signals

> data StrucSig a
>   = Definer a
>   | Alias3 (StrucSig a) String (Box (StrucSig a)) ClusterLoc

> data Port =
>   InPort Int | OutPort | Local Int
>   deriving Show

A structural signal contains an alternative semantics that describes
its behavior (for example, a stream).  This is returned by the
behavior function.

> behavior :: StrucSig a -> a
> behavior (Definer x) = x
> behavior (Alias3 x _ _ _) = behavior x

Defining primitive circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~

A primitive that takes j singleton inputs and produces k singleton
outputs can be defined conveniently by primjkStruc.

> prim01Struc :: Signal a => a -> String -> String -> StrucSig a

The constant signals can be defined as circuits with zero inputs and
one output.

> prim01Struc f lbl lblx =
>   let box =
>         Box { boxLbl      = lbl
>             , boxParent   = TopBox -- ???ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = []
>             , boxOutPorts = [xport]
>             , boxLocalSigs  = []
>             }
>       x     = Definer f
>       xport = Alias3 x "x" box (Outport (CluVar "x"))
>   in xport

> zeroStruc, oneStruc :: Signal a => StrucSig a
> zeroStruc = prim01Struc zero "zero_supply" "zero"
> oneStruc  = prim01Struc one  "one_supply"  "one"

> prim21Struc :: Signal a
>   => (a->a->a) -> String -> (String,String) -> String
>   -> StrucSig a -> StrucSig a -> StrucSig a

> prim21Struc f lbl (lbla,lblb) lblx a b =
>   let box =
>         Box { boxLbl      = lbl
>             , boxParent   = TopBox -- ???ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = [aport,bport]
>             , boxOutPorts = [xport]
>             , boxLocalSigs  = []
>             }
>       aport = Alias3 a lbla box (InportArg 0 (CluVar "aport"))
>       bport = Alias3 b lblb box  (InportArg 1 (CluVar "bport"))
>       x     = Definer (f (behavior a) (behavior b))
>       xport = Alias3 x lblx box (Outport (CluVar "x"))
>   in xport

Some structural logic gates
~~~~~~~~~~~~~~~~~~~~~~~~~~~

> and2Struc, or2Struc, nand2Struc, nor2Struc, xor2Struc
>   :: Signal a => StrucSig a -> StrucSig a -> StrucSig a

> and2Struc  = prim21Struc and2  "and2"  ("and2_i0","and2_i1")   "and2_x0"
> nand2Struc = prim21Struc nand2 "nand2" ("nand2_i0","nand2_i1") "nand2_x0"
> or2Struc   = prim21Struc or2   "or2"   ("or2_i0","or2_i1")     "or2_x0"
> nor2Struc  = prim21Struc nor2  "nor2"  ("nor2_i0","nor2_i1")   "nor2_x0"
> xor2Struc  = prim21Struc xor2  "xor2"  ("xor2_i0","xor2_i1")   "xor2_x0"

Defining bigger circuits
~~~~~~~~~~~~~~~~~~~~~~~~

> {-
> box22Struc
>   :: Signal a
>   => String -> String -> String -> String -> String -> Layout
>   -> (StrucSig a -> StrucSig a
>       -> (StrucSig a, StrucSig a))
>   -> StrucSig a -> StrucSig a -> (StrucSig a, StrucSig a)

> box22Struc boxlab alab blab xlab ylab layout f a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = undefined -- outtree
>             , boxLocSigs  = ps
>--             , boxWires    = []
>             }
>       ai    = Alias a alab InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       bi    = Alias b blab InPort btree
>       btree = Singleton (Just blab) (BoxInCluster box 1) bi
>       (x,y) = f ai bi
>       xout  = Alias x xlab OutPort xtree
>       xtree = Singleton (Just xlab) (ParentCluster outtree 0) xout
>       yout  = Alias y ylab OutPort ytree
>       ytree = Singleton (Just ylab) (ParentCluster outtree 1) yout
>       outtree = Tuple2 Nothing (BoxOutCluster box) xtree ytree
>       (bs,ps) = traceback [] [x,y] [] [ai,bi]
>   in (xout,yout)
> -}

> instance Signal a => Signal (StrucSig a) where
>   zero  = zeroStruc
>   one   = oneStruc
>   inv = error "not yet"
>   buf = error "not yet"
>   and2  = and2Struc
>   nand2 = nand2Struc
>   or2   = or2Struc
>   nor2  = nor2Struc
>   xor2  = xor2Struc
>   xnor2  = error "no"
>   xnor3  = error "no"
>   xnor4  = error "no"
>   or3  = error "not yet"
>   xor3  = error "not yet"
>   nand3  = error "not yet"
>   nor3  = error "not yet"
>   and3  = error "not yet"
>   or4  = error "not yet"
>   xor4  = error "not yet"
>   nand4  = error "not yet"
>   nor4  = error "not yet"
>   and4  = error "not yet"
>   alias = Alias3
