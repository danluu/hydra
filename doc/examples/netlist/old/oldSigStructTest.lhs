----------------------------------------------------------------------
	    The Hydra Digital Circuit Description Language
		Examples and Test Cases for SigStruct

Copyright (c) 2001 John O'Donnell.  See the README file for general
information, COPYING for the full copyright, and the web page for the
latest version: http://www.dcs.gla.ac.uk/~jtod/Hydra/
----------------------------------------------------------------------

> module SigStructTest where
> import Hydra
> import SigStruct

> import SigBool
> import SigStream
>-- import HydraCLI


See SigStructTestSave.lhs for older things that need to be updated and
inserted back here...

Example: Labeling and representing signal clusters
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

A circuit with singleton input and output clusters, using the bit
function.

> {- test1 :: Bool -> IO ()
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
>         return () -}

A circuit with a pair input and pair output, using the tuple2 function

> {- test22a :: (Bool,Bool) -> IO ()
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
>         return () -}

> circ22
>  :: Signal a
>  => StrucSig a
>  -> (StrucSig a, StrucSig a)
>  -> (StrucSig a, StrucSig a)

> circ22 witness =
>   let f (a,b) = (and2 a b, or2 a b)
>   in boxcluStruc1new witness "circ22"
>         ("abd","def") ("ghi","jkl") () f


> circWitless11
>  :: Signal a
>  => StrucSig a
>  -> StrucSig a

> circWitless11 =
>   let f (a,b) = (and2 a b, or2 a b)
>       g a = inv a
>   in boxcluStruc1newWitless "witless"
>         "abd" "def" () g

> circWitless21
>  :: Signal a
>  => (StrucSig a, StrucSig a)
>  -> StrucSig a

> circWitless21 =
>   let f (a,b) = and2 a b
>   in boxcluStruc1newWitless "witless"
>         ("abd","def") "ghi" () f

> circWitless12
>  :: Signal a
>  => StrucSig a
>  -> (StrucSig a, StrucSig a)

> circWitless12 =
>   let f a = (inv a, inv (inv a))
>   in boxcluStruc1newWitless "witless"
>         "abd" ("def","ghi") () f

> circWitless22
>  :: Signal a
>  => (StrucSig a, StrucSig a)
>  -> (StrucSig a, StrucSig a)

> circWitless22 =
>   let f (a,b) = (inv a, inv (inv b))
>   in boxcluStruc1newWitless "witless"
>         ("abd","def") ("ghi","jkl") () f


> foo :: Signal a => (StrucSig a, StrucSig a) -> (StrucSig a, StrucSig a)
> foo = circ22 zero

> test22b :: (Bool,Bool) -> IO ()
> test22b (a,b) =
>   do let inpa = Definer a (Singleton (Just "srca") undefined inpa)
>      let inpb = Definer b (Singleton (Just "srcb") undefined inpb)
>      let (x,y) = foo (inpa, inpb)
> --      putStrLn "test22b: pair input, pair output, with clusters"
> --      putStrLn ("input a = " ++ show (behavior inpa))
> --      putStrLn ("input b = " ++ show (behavior inpb))
> --      putStrLn ("result x = " ++ show (behavior x))
> --      putStrLn ("result y = " ++ show (behavior y))
>      let bx = sigOwner x
>      showboxquick 0 bx
>      showboxfull 0 bx
>      return ()

It seems to work!!!
....................
