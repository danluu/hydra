---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Transform where

This module takes a Hydra circuit specification module, and applies
the circuit structure transformation to it.  The result is a new
module which contains alternative versions of the declarations
augmented with unique labels.

> import Language.Haskell.THSyntax
> import Signal

---------------------------------------------------------------------------
			 Processing a Module
---------------------------------------------------------------------------

The transform_module function takes a computation that yields a list
of declarations comprising a module, and transforms each of them,
using mapQ.

> transform_module :: Q [Dec] -> Q [Dec]

> transform_module source =
>   do x <- source
>      qIO (putStrLn "~~~~~~~~~~~~~~~~~~~~~")
>      qIO (putStrLn "Transforming a Module")
>      qIO (putStrLn "~~~~~~~~~~~~~~~~~~~~~")
>      ys <- mapQ x []
>      return ys

---------------------------------------------------------------------------
		    Tools for Tracing and Testing
---------------------------------------------------------------------------

If requested to do so, the transformation functions will print the
data structures they are working on.

> qshowList :: Show a => Int -> [a] -> Q Exp

> qshowList i [] = return (Var [])  -- inelegant dummy return value
> qshowList i (x:xs) =
>   do qIO (putStrLn ("   " ++ show i ++ ". " ++ show x))
>      qshowList (i+1) xs

---------------------------------------------------------------------------
		       Constructing a Black Box
---------------------------------------------------------------------------

Black boxes are defined in the SigStruct module.  The following
functions are used by the transformation to construct black boxes for
circuit specifications.


The mkbox function takes the raw data known for a black box, and
constructs the full representation.  This function can be used
directly in a circuit specification, or it can occur in an application
generated by the automatic transformation (via bboxdef).

> mkbox :: Signal a => String -> [a] -> [a] -> [a] -> Box a
> mkbox boxname inps locs outs =
>   let parent = undefined
>       children = []
>   in Box
>        { boxLbl       = boxname
>        , boxInPorts   = inps
>        , boxOutPorts  = outs
>        , boxLocalSigs = locs
>        , boxParent    = parent
>        , boxChildren  = children
>        }

The bboxdef function generates an application of mkbox to the name of
the circuit and a list of the aliased signals within the box.

> bboxdef :: String -> [Exp] -> [Exp] -> [Exp] -> ExpQ
> bboxdef boxname xs ys zs =
>   let zinp = return (ListExp xs)
>       zequ = return (ListExp ys)
>       zout = return (ListExp zs)
>   in [| mkbox $(lift boxname) $(zinp) $(zequ) $(zout) |]

---------------------------------------------------------------------------
			      Renamings
---------------------------------------------------------------------------

The transformed circuit defines a black box data structure containing
a complete description of the circuit's structure.  The pieces of the
specification (for example, the input clusters) are transformed into
`renamed' versions; the renamed signals refer to data structures that
carry additional information, as well as the original signal.  For
example, the input pattern (x,y) might be transformed to (x',y') where
x' and y' are defined to be nodes containing the strings "x" and "y",
the locations of x and y in the circuit, and the actual values of x
and y.

This is done in two steps.  First, the original pattern or expression
is traversed in order to find the names that appear in it.  The
traversal keeps track of the position where a name appears in a
pattern or expression, and this information is encoded in a ClusterLoc
(cluster location) value.  Second, the original pattern or expression
is copied to form a new one, where the names are replaced with the new
versions.

The names that appear in a circuit specification need to be modified
by the transformation, and a Renaming value is used to control how
this is done.


A value of type Renaming contains the original name,
new name, new name for locally labelled signal.  The patterns for
signal definitions (both arguments to the function, and left hand
sides of equations) are traversed to find the signal definitions and
to construct the renaming table.

> type Renaming = (String, ClusterLoc, String, String)

Let r = ("x", loc, "x1", "x2") :: Renaming.  The fields of r have the
following meanings:
  "x" is the formal parameter name appearing in the source function definition
  "x1" is the formal parameter name used in the transformed code
  "x2" is the corresponding signal used inside the box; it is aliased to x1

For example, suppose the original definition is
   circ x ... = ...
Then the transformed definition will be
   circ x1 ... = ...
     where ... (x2 is used wherever x appears in the original)
           x2 = alias x1 "x" box (InportArg 0 (CluVar "x"))

Looking up a renaming
~~~~~~~~~~~~~~~~~~~~~

ren_lookup takes a function that determines whether x1 or x2 will be
returned when x is looked up.

  x is the name that appears in the original circuit specification

> ren_lookup :: (Renaming->String) -> String -> [Renaming] -> String
> ren_lookup f s [] = s
> ren_lookup f s (r:rs) =
>   if s == fst4 r
>     then f r
>     else ren_lookup f s rs

The ren_lookup_loc function takes a string s and a list of renamings
rs.  It looks for s in the renamings; an error is reported if it
doesn't appear (this would be a Hydra error, not an error in the
circuit specification).

> ren_lookup_loc ::String -> [Renaming] -> ExpQ
> ren_lookup_loc s [] = error "Can't find cluster location"
> ren_lookup_loc s (r:rs) =
>   if s == fst4 r
>     then cluster_loc_syn (snd4 r)
>     else ren_lookup_loc s rs

Constructing Aliases
~~~~~~~~~~~~~~~~~~~~

The following functions are used to build the equations for defining
aliases within a black box.

The mk_label function takes a string representing the name of a
signal, and generates code that creates the alias for it.  For
example, given a renaming r = ("a", CluVar "b", "c","d"), the
evaluation of mk_label [r] "a" will produce the declaration
  d = alias c "c" box (CluVar "b")

> test_mk_label :: Q ()
> test_mk_label =
>   do let r = ("a", CluVar "b", "c","d")
>      d <- (mk_label [r] "a")
>      qIO (putStrLn (show d))
>      return ()

*Transform> runQ test_mk_label
Val (Pvar "d")
    (Normal
      (App
        (App
          (App
             (App (Var "alias") (Var "c"))
             (Lit (String "c")))
          (Var "box"))
        (App (Con "CluVar") (Lit (String "b")))))
    []


> mk_label :: [Renaming] -> String -> Q Dec
> mk_label rs s =
>   do let s1 = ren_lookup fth4 s rs
>          s2 = ren_lookup thd4 s rs
>          loc' = con "CluTEMPcon" :: ExpQ
>          loc = ren_lookup_loc s rs  :: ExpQ
>      val (pvar s1)
>            (normal
>              (app (app (app (app (var "alias")
>                (var s2))
>                  (lit (String s2)))
>                    (var "box"))
>                      loc)) []

Building alias definitions
~~~~~~~~~~~~~~~~~~~~~~~~~~

The mk_all function generates aliases for all the signals used within
a black box.  This may not be right; can it make aliases for the
outputs as well as for the locals and formal inputs?

> mk_all :: [Renaming] -> [Dec] -> [String] -> Q [Dec]
> mk_all rs ds [] = return ds
> mk_all rs ds (s:ss) =
>   do d <- mk_label rs s
>      mk_all rs (d:ds) ss

The get_aliases function returns the list of aliased signals.  Why is
this not using rs at all???

> get_aliases :: [Renaming] -> [Exp] -> [String] -> Q [Exp]
> get_aliases rs vs [] = return vs
> get_aliases rs vs (s:ss) =
>   do v <- var s
>      get_aliases rs (v:vs) ss

---------------------------------------------------------------------------
		 Locations of Signals Within Clusters
---------------------------------------------------------------------------

The cluster_loc_syn function takes a signal, specified by its location
within a cluster, and returns a computation that will construct a
representation of that location.

> cluster_loc_syn :: ClusterLoc -> ExpQ
> cluster_loc_syn (InportArg i cl) =
>   app (app (con "InportArg") (lit (integerL (integer_int i))))
>     (cluster_loc_syn cl)
> cluster_loc_syn (Equation i cl) =
>   app (app (con "Equation") (lit (integerL (integer_int i))))
>     (cluster_loc_syn cl)
> cluster_loc_syn (CluVar s) = -- [| CluVar s |] produces junk!
>   app (con "CluVar") (string s)
> cluster_loc_syn (CluTup i j cl) =
>   app (app (app (con "CluTup") (lit (integerL (integer_int i))))
>     (lit (integerL (integer_int j)))) (cluster_loc_syn cl)

---------------------------------------------------------------------------
			  Building Renamings
---------------------------------------------------------------------------

The findvars_pat function searches a pattern, and locates all the
variables that appear in the pattern.  The result is returned in the
form of a list of Renamings, so that the located varaibles appear
along with their location in the pattern and their renamed version.

> findvars_pat :: [Renaming] -> (ClusterLoc->ClusterLoc) -> Pat -> Q [Renaming]
> findvars_pat rs f (Pvar s) =
>   if member s (map fst4 rs)
>     then return rs
>     else do s1 <- gensym s
>             s2 <- gensym s
>             let loc = f (CluVar s)
>             return ((s,loc,s1,s2) : rs)
> findvars_pat rs f (Ptup ps) =
>   do let n = length ps
>          g i p = f (CluTup n i p)
>      findvars_patlist rs 0 g ps
> findvars_pat rs _ _ = error "unimplemented pattern in findvars_pat"

This is for the list of elements in a tuple, as well as for the list
of arguments, or the list of equation left hand sides...

> findvars_patlist :: [Renaming]  -> Int -> (Int->ClusterLoc->ClusterLoc)
>    -> [Pat] -> Q [Renaming]
> findvars_patlist rs i f [] = return rs
> findvars_patlist rs i f (p:ps) =
>   do rs' <- findvars_pat rs (f i) p
>      findvars_patlist rs' (i+1) f ps

> findvars_exp :: [Renaming] -> (ClusterLoc->ClusterLoc) -> Exp -> Q [Renaming]
> findvars_exp rs f (Var s) =
>   do s1 <- gensym s
>      let loc = f (CluVar s)
>      return ((s,loc,s1,s1) : rs)
> findvars_exp rs f (Con s) = return rs
> findvars_exp rs f (Lit i) = return rs
> findvars_exp rs f (App e1 e2) =
>   do rs1 <- findvars_exp rs f e1
>      rs2 <- findvars_exp rs f e2
>      return (rs1++rs2++rs)
> findvars_exp rs f (Tup xs) =
>   do let n = length xs
>          g i p = f (CluTup n i p)
>      findvars_explist rs 0 g xs
> -- findvars_exp rs f (ListExp xs) =
> findvars_exp rs _ _ = error "findvars_exp unimplemented expression"

> findvars_explist :: [Renaming] -> Int -> (Int->ClusterLoc->ClusterLoc)
>   -> [Exp] -> Q [Renaming]
> findvars_explist rs i f [] = return rs
> findvars_explist rs i f (x:xs) =
>   do rs' <- findvars_exp rs (f i) x
>      findvars_explist rs' (i+1) f xs

---------------------------------------------------------------------------
		      Copying code with renaming
---------------------------------------------------------------------------

The following function copies a pattern, producing a new pattern with
the same structure but where names have been replaced according to the
renamings that are specified in the Renaming argument.  The first
argument, rs, is the list of renamings to be used.  The second
argument, f, is a function that takes a particular renaming, and
returns the name to be used.  A renaming contains two new names
corresponding to the original name, and the purpose of f is to choose
the appropriate one.  The last argument is a pattern to be renamed.

> update_pat :: [Renaming] -> (Renaming->String) -> Pat -> Pat

> update_pat rs f (Plit i)     = Plit i
> update_pat rs f (Pvar s)     = Pvar (ren_lookup f s rs)
> update_pat rs f (Ptup ps)    = Ptup (map (update_pat rs f) ps)
> update_pat rs f (Pcon s ps)  = Pcon s (map (update_pat rs f) ps)
> update_pat rs f (Ptilde p)   = Ptilde (update_pat rs f p)
> update_pat rs f (Paspat s p) = Paspat s (update_pat rs f p)
> update_pat rs f Pwild        = Pwild
> update_pat rs f p            = error "strange case of pattern"

The update_exp function is similar to update_pat, but it uses the
renaming table to replace the names that occur in an expression.

> update_exp :: [Renaming] -> (Renaming->String) -> Exp -> Exp

> update_exp rs f (Var s)      = Var (ren_lookup f s rs)
> update_exp rs f (Con s)      = Con s
> update_exp rs f (Lit i)      = Lit i
> update_exp rs f (App e1 e2)  = App (update_exp rs f e1) (update_exp rs f e2)
> update_exp rs f (Tup xs)     = Tup (map (update_exp rs f) xs)
> update_exp rs f (ListExp xs) = ListExp (map (update_exp rs f) xs)

> update_exp rs f (Lam _ _)    = error "lambdas unsupported"
> update_exp rs f (Cond _ _ _) = error "cond unsupported"
> update_exp rs f (Let _ _)    = error "let unsupported"
> update_exp rs f (Case _ _)   = error "case unsupported"
> update_exp rs f (Do _)       = error "do unsupported"
> update_exp rs f (Comp _)     = error "comp unsupported"
> update_exp rs f (ArithSeq _) = error "arith seq unsupported"
> update_exp rs f e            = error "strange case of expression" -- e

---------------------------------------------------------------------------
	       Transformation of Top Level Declarations
---------------------------------------------------------------------------

> transform_dec :: Dec -> Q Dec
> transform_dec (Fun nm [Clause pts (Normal exp) decs]) =
>   do qIO (putStrLn (take 75 (repeat '-')))
>      qIO (putStrLn ("*** Transforming declaration: Fun " ++ nm))
>      qIO (putStrLn (show ((Fun nm [Clause pts (Normal exp) decs]))))

> -- Find the pieces of the equations

>      let lhss = map (\(Val lhs rhs _) -> lhs) decs
>      let rhss = map (\(Val lhs (Normal rhs) _) -> rhs) decs

> -- Print the original source definition

>      qIO (putStrLn "InPort patterns:")
>      qshowList 1 pts
>      qIO (putStrLn ("Output expression:\n   " ++ show exp))
> --      qIO (putStrLn " Equations:")  -- raw form of equations
> --      qshowList 1 decs              -- raw form of equations
>      qIO (putStrLn "Left hand sides of the equations:")
>      qshowList 0 lhss
>      qIO (putStrLn "Right hand sides of the equations:")
>      qshowList 0 rhss

> -- Traverse source code to find the names and build their renamings
>      inp_rs <- findvars_patlist [] 0 InportArg pts
>      equ_rs <- findvars_patlist [] 0 Equation lhss
>      out_rs <- findvars_exp [] Outport exp
>      let rs = inp_rs ++ equ_rs -- not including output renamings
>      qIO (putStrLn "Renamings for inputs:")
>      qshowList 0 inp_rs
>      qIO (putStrLn "Renamings for local definitions:")
>      qshowList 0 equ_rs
>      qIO (putStrLn "Renamings for outputs:")
>      qshowList 0 out_rs

> -- Rename the patterns
>      let pts' = map (update_pat rs thd4) pts
>      qIO (putStrLn "Renamed InPort patterns:")
>      qshowList 1 pts'
>      let lhss' = map (update_pat rs thd4) lhss
>      qIO (putStrLn "Renamed left hand sides:")
>      qshowList 0 lhss'

> -- Rename the expressions
>      let exp' = update_exp rs fth4 exp
>      qIO (putStrLn ("Renamed output expression:\n   " ++ show exp'))
>      let rhss' = map (update_exp rs fth4) rhss
>      qIO (putStrLn "Renamed right hand sides:")
>      qshowList 0 rhss'

> -- Build the new equations
>      let eqs' = zipWith (\l r -> Val l (Normal r) []) lhss' rhss'
>      qIO (putStrLn "Renamed equations")
>      qshowList 0 eqs'

> -- Labelling equation
>      lab_eqs <- mk_all rs [] (map fst4 rs)

> -- Build the black box definition
>      aliases <- get_aliases rs [] (map fth4 rs)
>      inp_aliases <- get_aliases inp_rs [] (map fth4 inp_rs)
>      equ_aliases <- get_aliases equ_rs [] (map fth4 equ_rs)
>      out_aliases <- get_aliases out_rs [] (map fth4 equ_rs)
>      bbody <-  bboxdef nm inp_aliases equ_aliases out_aliases
>      qIO (putStrLn ("bbody = " ++ show bbody))
>      let boxdef = Val (Pvar "box") (Normal bbody) []

> -- Construct the final transformed function definition
>      let decs' = eqs' ++ lab_eqs ++ [boxdef]
>      let nm' = nm -- ++ "_transformed"
>      let f' = Fun nm' [Clause pts' (Normal exp') decs']

> -- Print the result
>      qIO (putStrLn "Final transformed specification")
>      qIO (putStrLn "Equations")
>      qshowList 1 decs'

> -- Finish up
>      qIO (putStrLn ("*** Ending transformation of " ++ nm))
>      return f'

> transform_dec (Proto nm ty) =
>   do
>      qIO (putStrLn (take 75 (repeat '-')))
>      qIO (putStrLn ("*** Transforming declaration: Proto " ++ nm))
>      qIO (putStrLn (show ty))
>      let r = Proto nm ty
>      return r

> transform_dec (Val p rhs decs)         = error "dec Val"
> transform_dec (Data cxt s ss cons ss2) = error "dec Data"
> transform_dec (Newtype _ _ _ _ _)      = error "dec Newtype"
> transform_dec (TySyn _ _ _)            = error "dec TySyn"
> transform_dec (Class _ _ _ _)          = error "dec Class"
> transform_dec (Instance _ _ _)         = error "dec Instance"
> transform_dec (Foreign _)              = error "dec Foreign"
> transform_dec _                        = error "dec strange case"

---------------------------------------------------------------------------
			  Basic Definitions
---------------------------------------------------------------------------

Look for standard prelude equivalents or more elegant ways to do it

> integer_int :: Int -> Integer
> integer_int i = toInteger i

> mapQ [] ys = return ys
> mapQ (x:xs) ys =
>   do y <- transform_dec x
>      mapQ xs (y:ys)

> member :: Eq a => a -> [a] -> Bool
> member x [] = False
> member x (y:ys)
>   | x==y = True
>   | otherwise = member x ys

> fst4 (a,b,c,d) = a
> snd4 (a,b,c,d) = b
> thd4 (a,b,c,d) = c
> fth4 (a,b,c,d) = d
