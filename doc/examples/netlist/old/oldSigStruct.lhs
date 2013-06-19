----------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
		Structural Representation of Circuits

Copyright (c) 2001 John O'Donnell.  See the README file for general
information, COPYING for the full copyright, and the web page for the
latest version: http://www.dcs.gla.ac.uk/~jtod/Hydra/
----------------------------------------------------------------------

> module SigStruct where
> import Hydra


SigStruct defines the structural representation of signals and black box
circuits.  These can be used to provide a variety of services that require
knowledge of the structure of a circuit as well as its semantics,
including the generation of netlists and the traversal circuits in order
to find signals or components.


Structured Signals
~~~~~~~~~~~~~~~~~~


Clustered Signals
~~~~~~~~~~~~~~~~~

A cluster is a nested group of signals, such as

  Signal a => (a, (a,[a]), a)

In order to traverse the elements of clusters, Hydra uses a cluster tree
to represent explicitly the structure of a cluster.  The tree has a type
parameter a which is the signal type.

> data ClusterTree a
>   = Singleton (Maybe String) (ClusterParent a)
>       (StrucSig a)
>   | Tuple2 (Maybe String) (ClusterParent a)
>       (ClusterTree a) (ClusterTree a)
>   | Tuple3 (Maybe String) (ClusterParent a)
>       (ClusterTree a) (ClusterTree a) (ClusterTree a)
>   | Word (Maybe String) (ClusterParent a)
>       Int [ClusterTree a]

A pointer from a tree node to its parent has a ClusterParent type,
where the type parameters a and b are the same as for ClusterTree.

> data ClusterParent a
>   = BoxInCluster (Box a) Int
>   | BoxOutCluster (Box a)
>   | ParentCluster (ClusterTree a) Int


Operations on Clusters
~~~~~~~~~~~~~~~~~~~~~~

> -- getSgl :: ClusterTree a -> a
> -- getSgl (Singleton _ _ x) = x
> -- getSgl _ = error "Not a singleton"

> cluSigs :: ClusterTree a -> [StrucSig a]
> cluSigs (Singleton _ _ x) = [x]
> cluSigs (Tuple2 _ _ x y) = cluSigs x ++ cluSigs y
> cluSigs (Tuple3 _ _ x y z) = cluSigs x ++ cluSigs y ++ cluSigs z
> cluSigs (Word _ _ _ xs) = concat (map cluSigs xs)

> cluname :: ClusterTree a -> String
> cluname (Singleton s _ x)  = maybeName s
> cluname (Tuple2 s p a b)   = maybeName s
> cluname (Tuple3 s p a b c) = maybeName s
> cluname (Word s p i xs)    = maybeName s

> showCluNames :: ClusterTree a -> String
> showCluNames (Singleton s _ x) = maybeName s
> showCluNames (Tuple2 s p a b)
>   = maybeName s ++ "=("
>     ++ showCluNames a
>     ++ "," ++ showCluNames b
>     ++ ")"
> showCluNames (Tuple3 s p a b c)
>   = maybeName s ++ "=("
>     ++ showCluNames a ++ "," ++ showCluNames b
>     ++ "," ++ showCluNames c
>     ++ ")"
> showCluNames (Word s p i xs)
>   = maybeName s ++ "[" ++ show i ++ "]"

f shows the alternative semantics of a signal

> showCluTree :: (StrucSig a -> String) -> ClusterTree a -> String
> showCluTree f (Singleton s _ x) = maybeName s ++ "=" ++ f x
> showCluTree f (Tuple2 s p a b)
>   = maybeName s ++ "="
>     ++ "(" ++ showCluTree f a ++ "," ++ showCluTree f b ++ ")"
> showCluTree f (Tuple3 s p a b c) = maybeName s
> showCluTree f (Word s p i xs) = maybeName s

> maybeName :: Maybe String -> String
> maybeName Nothing = "_"
> maybeName (Just s) = s


Operations on Cluster Parents
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> clusterParent :: ClusterTree a -> ClusterParent a
> clusterParent (Singleton _ p _)  = p
> clusterParent (Tuple2 _ p _ _)   = p
> clusterParent (Tuple3 _ p _ _ _) = p
> clusterParent (Word _ p _ _)     = p

> showClusterParent :: ClusterParent a -> String
> showClusterParent (ParentCluster clu i)
>   = cluname clu
> showClusterParent (BoxInCluster box i)
>   = "boxport" ++ show i
> showClusterParent (BoxOutCluster box)
>   = "box OutPort"


Representation of Black Box Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Box type describes the structure of a black box circuit with
signals of type StrucSig a, where a is called the base signal type.

> data Signal a => Box a
>   = Box
>      { boxSpecies  :: String
>      , boxLayout   :: Layout
>      , boxParent   :: BoxParent a
>      , boxChildren :: [Box a]
>      , boxInPorts  :: [ClusterTree a]
>      , boxOutPorts :: ClusterTree a
>      , boxSignals  :: [StrucSig a]
> --     , boxWires    :: [Wire (StrucSig a b)]
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

> data BoxParent a
>   = TopBox
>   | ChildBox (Box a) Int

The boxChildren field contains a list of the black boxes used
within the box.  Only circuits mentioned explicitly in the box
definition are listed; circuits that are more deeply nested do not
appear in boxChildren, but they can be obtained by a recursive
traversal.


Structural Representation of Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A structural signal describes exactly where in the circuit the signal
is found.  It is represented using the StrucSig type, which decorates
a base signal of type a with additional structural information.  The
signal location is specified by a reference to a ClusterTree.

> data StrucSig a
>   = Definer a (ClusterTree a)
>   | Alias (StrucSig a) Port (ClusterTree a)

Aliased signals are marked according to their status within a box,
using the Port type.  A Tag is used to mark a point on feedback
loops, to prevent structure traversals from falling into infinite
recursions.

> data Port =
>   InPort | OutPort | BiPort | Local | BiLocal | Tag Int
>   deriving Show


Behavioral Semantics of Structural Signals
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A structural signal contains an alternative semantics that describes
its behavior (for example, a stream).  This is returned by the
behavior function.

> behavior :: StrucSig a -> a
> behavior (Definer x _) = x
> behavior (Alias x _ _) = behavior x


Traversing a circuit graph
~~~~~~~~~~~~~~~~~~~~~~~~~~

There are three groups of signal owned by a box: its input ports,
its output ports, and signals that are internal to the box (these
must be outputs produced by children of the box).  These are all
available in the boxPorts field.

> child :: Signal a => Box a -> Int -> Box a
> child b i = boxChildren b !! i

Every signal belongs to exactly one black box which is called the
`owner' of the signal.  From the viewpoint of a black box, every
signal that belongs to it is either an Input, and Output, or a
Internal signal.

> sigOwner :: Signal a => StrucSig a -> Box a
> sigOwner = clusterTreeOwner . sigClusterTree

Each structural signal exists within a cluster, which has a
corresponding cluster tree.  The following function obtains that
tree.

> sigClusterTree :: StrucSig a -> ClusterTree a
> sigClusterTree (Definer _ c) = c
> sigClusterTree (Alias _ _ c) = c

Cluster trees allow flexible traversal, since they contain pointers
going both up and down.  The clusterTreeRoot function returns the
node at the root of a tree; this in turn points up to the black box
that owns the cluster.

> clusterTreeRoot :: Signal a => ClusterTree a -> ClusterTree a
> clusterTreeRoot c =
>   case clusterParent c of
>     ParentCluster c' i -> clusterTreeRoot c'
>     BoxInCluster b i   -> c
>     BoxOutCluster b    -> c

Given a cluster tree node, returns the black box that owns the
cluster.  clupbox

> clusterTreeOwner :: Signal a => ClusterTree a -> Box a
> clusterTreeOwner c =
>   case clusterParent c of
>     ParentCluster c' i -> clusterTreeOwner c'
>     BoxInCluster b i   -> b
>     BoxOutCluster b    -> b

> outPortSource :: StrucSig a -> StrucSig a
> outPortSource (Alias x OutPort p) = x
> outPortSource (Alias x _ p) = error "not an OutPort"
> outPortSource _ = error "not an Alias"

> inPortSource :: StrucSig a -> StrucSig a
> inPortSource (Alias x InPort p) = x
> inPortSource (Alias x _ p) = error "not an InPort"
> inPortSource _ = error "not an Alias"

> boxinputs :: Signal a => Box a -> [StrucSig a]
> boxinputs b =
>   map inPortSource
>     (concat (map cluSigs (boxInPorts b)))


Showing Signals, Clusters and Boxes
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

There are several ways to show a signal (that is, produce a
descriptive character string).

> showcluster :: Int -> ClusterTree a -> String
> showcluster i x = "port " ++ show i ++ ": " ++ showCluNames x

> describe_sig :: StrucSig a -> String
> describe_sig (Definer x p) = cluname p
> describe_sig (Alias x port p) = show port ++ " " ++ cluname p

> sigName :: Signal a => StrucSig a -> String
> sigName = cluname . sigClusterTree

> describe_sig_in_box :: Signal a => StrucSig a -> String
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

> showHandle :: Signal a => StrucSig a -> IO ()
> showHandle x = showboxfull 0 (sigOwner x)

> showboxquick :: Signal a => Int -> Box a -> IO ()
> showboxquick i b =
>   do putStrLn (indent i ++ "Summary of " ++ boxSpecies b)
>      putStrLn (indent i ++ "  input ports:" ++
>             concat (map ((' ':) . showCluNames) (boxInPorts b)))
>      putStrLn (indent i ++ "  input sources:" ++
>             concat (map ((' ':) . describe_sig) (boxinputs b)))
>      putStrLn (indent i ++ "  output ports: "
>                  ++ showCluNames (boxOutPorts b))

> showboxfull :: Signal a => Int -> Box a -> IO ()
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

> boxEqInBox :: Signal a => Box a -> Box a -> Bool
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

> sigEqInBox :: Signal a => StrucSig a -> StrucSig a -> Bool
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

> cmpCluLoc :: Signal a =>
>   ClusterTree a -> ClusterTree a -> Maybe (Box a, Box a)

> cmpCluLoc (Singleton _ p1 _) (Singleton _ p2 _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Tuple2 _ p1 _ _) (Tuple2 _ p2 _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Tuple3 _ p1 _ _ _) (Tuple3 _ p2 _ _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc (Word _ p1 _ _) (Word _ p2 _ _) =
>   cmpParentLoc p1 p2
> cmpCluLoc _ _ = Nothing

> cmpParentLoc :: Signal a =>
>   ClusterParent a -> ClusterParent a -> Maybe (Box a, Box a)

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
>   -> [StrucSig a]
>   -> [Box a]
>   -> [StrucSig a]
>   -> ([Box a], [StrucSig a])

> member x [] = False
> member x (y:ys) = if x==y then True else member x ys

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
>   => a -> String -> String -> Layout
>   -> StrucSig a

> prim01Struc x ylab boxlab layout =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxInPorts  = []
>             , boxOutPorts = ytree
>             , boxSignals  = []
>--             , boxWires    = []
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
>   => (a->a) -> String -> String -> String -> Layout
>   -> StrucSig a -> StrucSig a

> prim11Struc f laba labx boxlab layout a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = []
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>--             , boxWires    = []
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
>   => (a->a->a) -> [String] -> String -> String -> Layout
>   -> StrucSig a -> StrucSig a -> StrucSig a

> prim21Struc f [laba,labb] labx boxlab layout a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ???ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>--             , boxWires    = []
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
>   => (a->a->a->a) -> [String] -> String -> String -> Layout
>   -> StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a

> prim31Struc f [laba,labb,labc] labx boxlab layout a b c =
>   let box =
>         Box {boxSpecies   = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ??? ChildOfBox (sigOwner a)
>             , boxChildren = []
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = xtree
>             , boxSignals  = []
>--             , boxWires    = []
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

> conStruc :: Signal a => String -> a -> StrucSig a
> conStruc l x = prim01Struc x l "Constant" undefined

Inputs to a circuit

> input :: Signal a => String -> a -> StrucSig a
> input l x = prim01Struc x l "Input" undefined

The buffer and inverter

> bufStruc, invStruc :: Signal a => StrucSig a -> StrucSig a
> bufStruc = prim11Struc buf "a" "x" "buf" undefined
> invStruc = prim11Struc inv "a" "x" "inv" undefined

> dffStruc :: (Signal a, Clocked a) => StrucSig a -> StrucSig a
> dffStruc = prim11Struc dff "dff_ld" "dff_a" "dff" undefined

Basic logic gates

> and2Struc, nand2Struc, or2Struc, nor2Struc, xor2Struc
>   :: Signal a
>   => StrucSig a -> StrucSig a -> StrucSig a
> and2Struc = prim21Struc and2
>               ["and2_a","and2b"] "and2_x" "and2" undefined
> nand2Struc = prim21Struc nand2
>               ["nand2_a","nand2b"] "nand2_x" "nand2" undefined
> or2Struc  = prim21Struc or2
>               ["or2_a","or2_b"] "or2_x" "or2"  undefined
> nor2Struc  = prim21Struc nor2
>               ["nor2_a","nor2_b"] "nor2_x" "nor2"  undefined
> xor2Struc = prim21Struc xor2
>               ["xor2_a","xor2_b"] "xor2_x" "xor2" undefined

> and3Struc, nand3Struc, or3Struc, nor3Struc, xor3Struc
>   :: Signal a
>   => StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a
> and3Struc = prim31Struc and3
>               ["and3_a","and3_b","and3_c"] "and3_x" "and3"
>               undefined
> nand3Struc = prim31Struc nand3
>               ["nand3_a","nand3_b","nand3_c"] "nand3_x" "nand3"
>               undefined
> or3Struc  = prim31Struc or3
>               ["or3_a","or3_b","or3_c"] "or3_x" "or3"  undefined
> nor3Struc  = prim31Struc nor3
>               ["nor3_a","nor3_b","nor3_c"] "nor3_x" "nor3"  undefined
> xor3Struc  = prim31Struc xor3
>               ["xor3_a","xor3_b","xor3_c"] "xor3_x" "xor3"  undefined

> and4Struc, nand4Struc, or4Struc, nor4Struc, xor4Struc
>   :: Signal a
>   => StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a

> and4Struc = error "unimplemented"
> nand4Struc = error "unimplemented"
> or4Struc = error "unimplemented"
> nor4Struc = error "unimplemented"
> xor4Struc = error "unimplemented"

Signal Instances for Standard Structural Circuits
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> instance Signal a => Signal (StrucSig a)
>   where
>     zero  = conStruc "0" zero
>     one   = conStruc "1" one
>     buf   = bufStruc
>     inv   = invStruc

>     and2  = and2Struc
>     or2   = or2Struc
>     nand2 = and2Struc
>     nor2  = or2Struc
>     xor2  = xor2Struc

>     and3  = and3Struc
>     or3   = or3Struc
>     nand3 = and3Struc
>     nor3  = or3Struc
>     xor3  = xor3Struc

>     and4  = and4Struc
>     or4   = or4Struc
>     nand4 = and4Struc
>     nor4  = or4Struc
>     xor4  = xor4Struc


> instance Clocked a => Clocked (StrucSig a) where
>   dff = dffStruc

> instance Signal a => Structured (StrucSig a) where
>   box11 = box11Struc
>   box21 = box21Struc
>   box22 = undefined -- box22Struc
>   box31 = box31Struc
>   box41 = undefined --box41Struc



Constructing Black Box Circuits with Unclustered Ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> box11Struc
>   :: Signal a
>   => String -> String -> String -> Layout
>   -> (StrucSig a -> StrucSig a)
>   -> StrucSig a -> StrucSig a

> box11Struc boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>--             , boxWires    = []
>             }
>       ai    = Alias a InPort atree
>       atree = Singleton (Just alab) (BoxInCluster box 0) ai
>       x     = f ai
>       xout  = Alias x OutPort xtree
>       xtree = Singleton (Just xlab) (BoxOutCluster box) xout
>       (bs,ps) = traceback [] [x] [] [ai]
>   in xout

> box21Struc
>   :: Signal a
>   => String -> String -> String -> String -> Layout
>   -> (StrucSig a -> StrucSig a -> StrucSig a)
>   -> StrucSig a -> StrucSig a -> StrucSig a

> box21Struc boxlab alab blab xlab layout f a b =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>--             , boxWires    = []
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
>             , boxOutPorts = outtree
>             , boxSignals  = ps
>--             , boxWires    = []
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
>   => String -> String -> String -> String -> String -> Layout
>   -> (StrucSig a -> StrucSig a  -> StrucSig a
>       -> StrucSig a)
>   -> StrucSig a -> StrucSig a -> StrucSig a -> StrucSig a

> box31Struc boxlab alab blab clab xlab layout f a b c =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>--             , boxWires    = []
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
>   => String -> String -> String -> String -> String -> String -> Layout
>   -> (StrucSig a -> StrucSig a -> StrucSig a
>       -> (StrucSig a, StrucSig a))
>   -> StrucSig a -> StrucSig a -> StrucSig a
>     -> (StrucSig a, StrucSig a)

> box32Struc boxlab alab blab clab xlab ylab layout f a b c =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox -- ChildOfBox (sigOwner a)
>             , boxChildren = bs
>             , boxInPorts  = [atree,btree,ctree]
>             , boxOutPorts = outtree
>             , boxSignals  = ps
>--             , boxWires    = []
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



Labelling and Representation of Signal Clusters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> clbit
>   :: String
>   -> Port
>   -> ClusterParent a
>   -> StrucSig a
>   -> (StrucSig a, ClusterTree a)

> clbit label port parent x =
>   let x' = Alias x port clutree
>       clutree = Singleton (Just label) parent x'
>   in (x', clutree)

> tuple2
>   :: String
>   -> (Port -> ClusterParent a -> c -> (c, ClusterTree a))
>   -> (Port -> ClusterParent a -> d -> (d, ClusterTree a))
>   -> Port
>   -> ClusterParent a
>   -> (c,d)
>   -> ((c,d), ClusterTree a)

> tuple2 label f g port parent (x,y) =
>   let tree = Tuple2 (Just label) parent xtree ytree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>   in ((x',y'), tree)

> tuple3
>   :: String
>   -> (Port -> ClusterParent a -> c -> (c, ClusterTree a))
>   -> (Port -> ClusterParent a -> d -> (d, ClusterTree a))
>   -> (Port -> ClusterParent a -> e -> (e, ClusterTree a))
>   -> Port
>   -> ClusterParent a
>   -> (c,d,e)
>   -> ((c,d,e), ClusterTree a)

> tuple3 label f g h port parent (x,y,z) =
>   let tree = Tuple3 (Just label) parent xtree ytree ztree
>       (x',xtree) = f port (ParentCluster tree 0) x
>       (y',ytree) = g port (ParentCluster tree 1) y
>       (z',ztree) = h port (ParentCluster tree 1) z
>   in ((x',y',z'), tree)


Overloaded Labelling and Representation of Signal Clusters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The Cluster class defines a function buildCluster that constructs labelled
clusters and corresponding cluster tree prepresentations in a uniform
manner, making it easier to define black box circuits.

> class Cluster a b c | c->a, c->b where
>   buildCluster :: b -> Port -> ClusterParent a
>     -> c -> (c, ClusterTree a)

> instance Cluster a [Char] (StrucSig a) where
>   buildCluster label port parent x = clbit label port parent x

> instance (Cluster a b c, Cluster a d e)
>   => Cluster a (b,d) (c,e) where
>    buildCluster (xlab,ylab) port parent (x,y) =
>      tuple2 "*"
>         (buildCluster xlab)
>         (buildCluster ylab)
>         port parent (x,y)


Operations on Cluster Trees
~~~~~~~~~~~~~~~~~~~~~~~~~~~

> showCluRep :: Show a => ClusterTree a -> String
> showCluRep (Singleton s _ x) = strucSigStruc x
> showCluRep (Tuple2 s p a b) =
>   "(" ++ showCluRep a ++ ", " ++ showCluRep b ++ ")"
> showCluRep _ = error "bad case"

> strucSigStruc :: Show a => StrucSig a -> String
> strucSigStruc (Definer x _) = "Definer-" ++ show x
> strucSigStruc (Alias x p _) =
>    "Alias-" ++ show p ++ " " ++ strucSigStruc x

The outSources function takes an output cluster tree and returns the list
of underlying output signals, which can then be used to trace back through
the contents of a black box.  Note that each signal in the output cluster
tree has an OutPort alias, which must be skipped.

> outSources :: Signal a => ClusterTree a -> [StrucSig a]

> outSources (Singleton _ _ x) =
>   case x of
>     Alias x OutPort cp -> [x]
>     _ -> error "outSources: invalid OurPort alias"
> outSources (Tuple2 _ _ x y) =
>   outSources x ++ outSources y
> outSources (Tuple3 _ _ x y z) =
>   outSources x ++ outSources y ++ outSources z
> outSources _ = error "outSources: unimplemented case"


Constructing  black boxes with clustered ports
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The label tuples are passed directly into the black box building function,
where it can use buildCluster itself.  In order to avoid the "ambiguous
type" error, a signal witness is passed to the function.

> boxcluStruc1new
>   :: (Signal a,
>       Cluster a c d,
>       Cluster a e f)
>   => StrucSig a  -- signal witness
>   -> String   -- box label
>   -> c        -- input label
>   -> e        -- output label
>   -> Layout  -- layout
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
>--             , boxWires    = []
>             }
>       foo = witness : ps
>       (a',atree) = buildCluster alab InPort (BoxInCluster box 0) a
>       x = f a'
>       (x',xtree) = buildCluster xlab OutPort (BoxOutCluster box) x
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x'

> boxcluStruc1newWitless
>   :: (Signal a,
>       Cluster a c d,
>       Cluster a e f)
>   => String   -- box label
>   -> c        -- input label
>   -> e        -- output label
>   -> Layout  -- layout
>   -> (d->f)   -- behavior
>   -> d        -- input
>   -> f        -- output

> boxcluStruc1newWitless boxlab alab xlab layout f a =
>   let box =
>         Box { boxSpecies  = boxlab
>             , boxLayout   = layout
>             , boxParent   = TopBox
>             , boxChildren = bs
>             , boxInPorts  = [atree]
>             , boxOutPorts = xtree
>             , boxSignals  = ps
>--             , boxWires    = []
>             }
>       (a',atree) = buildCluster alab InPort (BoxInCluster box 0) a
>       x = f a'
>       (x',xtree) = buildCluster xlab OutPort (BoxOutCluster box) x
>       (bs,ps) = traceback [] (outSources xtree) [] []
>   in x'


