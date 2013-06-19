---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Transform where

> import Language.Haskell.THSyntax
> import SigStruct

Go through all the definitions in the file, and transform them one by one.

> transform_module :: Q [Dec] -> Q [Dec]
> transform_module source =
>   do x <- source
>      qIO (putStrLn "* bar")
>      ys <- mapQ x []
>      return ys

> mapQ [] ys = return ys
> mapQ (x:xs) ys =
>   do y <- transform_circ x
>      mapQ xs (y:ys)

The main transformation function.

> transform_circ :: Dec -> Q Dec
> transform_circ (Fun nm [Clause pts (Normal exp) decs]) =
>   do
>      qIO (putStrLn (take 75 (repeat '-')))
>      qIO (putStrLn ("*** Transforming " ++ nm))

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

> -- Traverse patterns to find the names and build their renamings
>      rs <- findvarsList [] 0 InportArg pts
>      rs <- findvarsList rs 0 Equation lhss
>      qIO (putStrLn "Renamings for signals:")
>      qshowList 0 rs

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

> -- Build the black box definition
>      bbody <-  bboxdef []
>      qIO (putStrLn ("bbody = " ++ show bbody))
>      let boxdef = Val (Pvar "box") (Normal bbody) []

> -- Labelling equation
>      lab_eqs <- mk_all rs [] (map fst4 rs)

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
> transform_circ _ = error "dec isn't function in proper form"

Build the equations for defining aliases

> mk_label :: [Renaming] -> String -> Q Dec
> mk_label rs s =
>   do let s1 = ren_lookup fth4 s rs
>          s2 = ren_lookup thd4 s rs
>          loc' = con "CluTEMPcon" :: ExpQ
>          loc = ren_lookup_loc s rs  :: ExpQ
>      val (pvar s1)
>            (normal
>             (app (app (app (app (con "Alias3")
>                (var s2))
>                  (lit (String s2)))
>                    (var "box"))
>                      loc)) []

> mk_all :: [Renaming] -> [Dec] -> [String] -> Q [Dec]
> mk_all rs ds [] = return ds
> mk_all rs ds (s:ss) =
>   do d <- mk_label rs s
>      mk_all rs (d:ds) ss

A Renaming object consists of original name, new name, new name for
locally labelled signal.  The patterns for signal definitions (both
arguments to the function, and left hand sides of equations) are
traversed to find the signal definitions and to construct the renaming
table.


> type Renaming = (String, ClusterLoc, String, String)

The following functions update a pattern and expression by performing
the renamings that have been prepared in the Renaming argument.

> update_pat :: [Renaming] -> (Renaming->String) -> Pat -> Pat
> update_pat rs f (Plit i) = Plit i
> update_pat rs f (Pvar s) = Pvar (ren_lookup f s rs)
> update_pat rs f (Ptup ps) = Ptup (map (update_pat rs f) ps)
> update_pat rs f (Pcon s ps) = Pcon s (map (update_pat rs f) ps)
> update_pat rs f (Ptilde p) = Ptilde (update_pat rs f p)
> update_pat rs f (Paspat s p) = Paspat s (update_pat rs f p)
> update_pat rs f Pwild = Pwild
> -- update_pat rs f p = p  -- shouldn't be needed

> update_exp :: [Renaming] -> (Renaming->String) -> Exp -> Exp
> update_exp rs f (Var s) = Var (ren_lookup f s rs)
> update_exp rs f (Con s) = Con s
> update_exp rs f (Lit i) = Lit i
> update_exp rs f (App e1 e2) = App (update_exp rs f e1) (update_exp rs f e2)
> update_exp rs f (Lam _ _) = error "lambdas unsupported"
> update_exp rs f (Tup xs) = Tup (map (update_exp rs f) xs)
> update_exp rs f (Cond _ _ _) = error "cond unsupported"
> update_exp rs f (Let _ _) = error "let unsupported"
> update_exp rs f (Case _ _) = error "case unsupported"
> update_exp rs f (Do _) = error "do unsupported"
> update_exp rs f (Comp _) = error "comp unsupported"
> update_exp rs f (ArithSeq _) = error "arith seq unsupported"
> update_exp rs f (ListExp xs) = ListExp (map (update_exp rs f) xs)
> update_exp rs f e = e


> ren_lookup :: (Renaming->String) -> String -> [Renaming] -> String
> ren_lookup f s [] = s
> ren_lookup f s (r:rs) =
>   if s == fst4 r
>     then f r
>     else ren_lookup f s rs

> ren_lookup_loc ::String -> [Renaming] -> ExpQ
> ren_lookup_loc s [] = error "Can't find cluster location"
> ren_lookup_loc s (r:rs) =
>   if s == fst4 r
>     then cluster_loc_syn (snd4 r)
>     else ren_lookup_loc s rs

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
> -- cluster_loc_syn CluTEMPcon = [| CluTEMPcon |]

> integer_int :: Int -> Integer
> integer_int i = toInteger i

> findvars :: [Renaming] -> (ClusterLoc->ClusterLoc) -> Pat -> Q [Renaming]
> findvars rs f (Pvar s) =
>   if member s (map fst4 rs)
>     then return rs
>     else do s1 <- gensym s
>             s2 <- gensym s
>             let loc = f (CluVar s)
>             return ((s,loc,s1,s2) : rs)
> findvars rs f (Ptup ps) =
>   do let n = length ps
>          g i p = f (CluTup n i p)
>      findvarsList rs 0 g ps
> findvars rs _ _ = error "unimplemented pattern in findvars"

This is for the list of elements in a tuple, as well as for the list
of arguments, or the list of equation left hand sides...

> findvarsList :: [Renaming]  -> Int -> (Int->ClusterLoc->ClusterLoc)
>    -> [Pat] -> Q [Renaming]
> findvarsList rs i f [] = return rs
> findvarsList rs i f (p:ps) =
>   do rs' <- findvars rs (f i) p
>      findvarsList rs' (i+1) f ps

> blackbox = undefined

> -- mkLabelledSig :: (String,String,Port) -> ExpQ
> -- mkLabelledSig (x,x',l) = [| Alias x x' box l |]

> bboxdef :: [String] -> ExpQ
> bboxdef ns =
>   app (var "blackbox") (listExp (map var ns))


----------------------------------------------------------------------
Ordinary functions, look for standard prelude equivalents or more
elegant ways to do it
----------------------------------------------------------------------

> qshowList :: Show a => Int -> [a] -> Q Exp
> qshowList i [] = return (Var [])  -- inelegant dummy return value
> qshowList i (x:xs) =
>   do qIO (putStrLn ("   " ++ show i ++ ". " ++ show x))
>      qshowList (i+1) xs

> member :: Eq a => a -> [a] -> Bool
> member x [] = False
> member x (y:ys)
>   | x==y = True
>   | otherwise = member x ys

> fst4 (a,b,c,d) = a
> snd4 (a,b,c,d) = b
> thd4 (a,b,c,d) = c
> fth4 (a,b,c,d) = d


> put = qIO . putStr


----------------------------------------------------------------------
Old things that are commented out...
----------------------------------------------------------------------

> {-

> reg1 :: (Signal a, Clocked a) => a -> a -> a
> reg1 =
>   box21 "reg1" "ld""a" "x" undefined f
>   where f ld a =
>           let x = dff (mux1 ld x a)
>           in x

> -- test :: IO ()
> test =
>   do x <- circ_defs_rep
>      y <- transform_circ (head x)
>      return ()

> test1 =
>   do x <- test
>      return ()

> -- try printing an expression
> --      let testExp = Var "this_is_a_variable"
> --      qIO (putStrLn (show testExp))

> -- Find the signal names  (probably don't need this with renamings now...)
>      let argnames = [n | Pvar n <- pts]
>      let equnames = [n | Val (Pvar n) _ _ <- decs]
>      let signames =
>            [(n, n++"_", InPort i) | (n,i) <- zip argnames [1..]]
>            ++ [(n, n++"_", Local i) | (n,i) <- zip equnames [1..]]
>      qIO (putStrLn " Signal names:")
>      qshowList 1 signames
>      let newsigvarnames = map (\(a,b,c)->b) signames

> type Renaming = (String,String,String)

> findvars :: [Renaming] -> Pat -> Q [Renaming]
> findvars rs (Pvar s) =
>   if member s (map fst3 rs)
>     then return rs
>     else do s1 <- gensym s
>             s2 <- gensym s
>             return ((s,s1,s2) : rs)
> findvars rs _ = error "unimplemented pattern in findvars"

> fst3 (a,b,c) = a
> snd3 (a,b,c) = b
> thd3 (a,b,c) = c

> findvarsList2 :: [Renaming2] -> Int
>    -> (Int->ClusterLoc->ClusterLoc) -> [Pat] -> Q [Renaming2]
> findvarsList2 rs i f [] = return rs
> findvarsList2 rs i f (p:ps) =
>   do rs' <- findvars2 rs (f i) p
>      findvarsList2 rs' (i+1) f ps

> findvarsList :: [Renaming] -> [Pat] -> Q [Renaming]
> findvarsList rs [] = return rs
> findvarsList rs (p:ps) =
>   do rs' <- findvars rs p
>      findvarsList rs' ps

> -}

