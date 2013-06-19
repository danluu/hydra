---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module FastAdd where

> import Signal
> import SigBool
> import Pattern
> import BitComb

A log time carry lookahead adder can be derived formally, starting
from a linear time ripple carry adder.  This module contains the
definitions used in the course of the derivation.

The full story is presented in a paper by John O'Donnell and Gudula
Ruenger.  The full paper can be found on the web at

	http://www.dcs.gla.ac.uk/~jtod/papers/2001_Adder/



This is a Haskell 98 program that accompanies the paper "Derivation of
a Carry Lookahead Addition Circuit".  This paper begins by defining a
linear-time ripple carry addition circuit, and then transforms it
through a sequence of stages into a log-time carry lookahead adder.

Each of the five versions of the adder circuit derived in the paper
appears below.  You can run them all on some sample test data by
executing the "main" IO action.  Save this file as Adder.hs, then
start Hugs or ghci, load the program by entering ":l Adder" and run it
by entering "main":

  ghci
    ....
  Prelude> :l Adder
    ....
  Main> main
    {... simulated output from the five versions of the adder circuit...}



--------------------------------------------------------------------
		       The Binary Represention
---------------------------------------------------------------------

> type Nat = Int
> type Bit = Bool

> bit :: Bit -> Nat
> bit False = 0
> bit True  = 1

> toBit :: Nat -> Bit
> toBit i
>   | i==0 = False
>   | i==1 = True
>   | otherwise = error "toBit: bad argument"

> bin :: [Bit] -> Nat
> bin xs =
>  let n = length xs
>  in sum [bit x * 2^(n-1-i) | (i,x) <- zip [0..n-1] xs]

> represent :: Nat -> Nat -> [Bit]
> represent n i =
>   let f i
>         | i==0  = []
>         | i>0   = f (i `div` 2)  ++ [toBit (i `mod` 2)]
>       bs = f i
>   in take (n - length bs) (repeat False) ++ bs

--------------------------------------------------------------------
 			    Fold and Scan
---------------------------------------------------------------------

> -- foldr :: (b->a->a) -> a -> [b] -> a
> -- foldr f a [] = a
> -- foldr f a (x:xs) = f x (foldr f a xs)

> wscanr :: (b->a->a) -> a -> [b] -> [a]
> wscanr f a xs =
>   [foldr f a (drop (i+1) xs) | i <- [0 .. length xs -1]]

> ascanr ::  (b->a->a) -> a -> [b] -> (a,[a])
> ascanr f a [] = (a,[])
> ascanr f a (x:xs) =
>   let (a',xs') = ascanr f a xs
>       a'' = f x a'
>   in (a'', a':xs')


--------------------------------------------------------------------
 			  Ripple Carry Adder
---------------------------------------------------------------------

> add1 :: Signal a => a -> [(a,a)] -> (a,[a])
> add1 c zs =
>  let (c',cs) = ascanr bcarry c zs
>      ss = zipWith bsum zs cs
>   in (c',ss)

--------------------------------------------------------------------
 			  Scan Compose Adder
---------------------------------------------------------------------

> apply f x = f x

> add2 :: Signal a => a -> [(a,a)] -> (a,[a])
> add2 c zs =
>   let ps = map bcarry zs
>       (cf,cfs) = ascanr (.) id ps
>       cs = zipWith apply cfs (repeat c)
>       c' = cf c
>       ss = zipWith bsum zs cs
>   in (c', ss)

--------------------------------------------------------------------
 		   Symbolic Function Compose Adder
---------------------------------------------------------------------

> data Sym = K | P | G

> bcarrySym :: Signal a => (a,a) -> Sym
> bcarrySym (x,y)
>   | is0 x && is0 y  = K
>   | is0 x && is1 y  = P
>   | is1 x && is0 y  = P
>   | is1 x && is1 y  = G

> applySym :: Signal a => Sym -> a -> a
> applySym K x = zero
> applySym P x = x
> applySym G x = one

> composeSym :: Sym -> Sym -> Sym
> composeSym K f = K
> composeSym P f = f
> composeSym G f = G

> add3 :: Signal a => a -> [(a,a)] -> (a,[a])
> add3 c zs =
>   let ps = map bcarrySym zs
>       (cf,cfs) = ascanr composeSym P ps
>       cs = zipWith applySym cfs (repeat c)
>       c' = applySym cf c
>       ss = zipWith bsum zs cs
>   in (c', ss)

--------------------------------------------------------------------
		       Tree Structured Circuits
---------------------------------------------------------------------

> data Tree a = Leaf a | Node (Tree a) (Tree a)
>   deriving Show

> mkTree :: Nat -> Tree ()
> mkTree n =
>   if n==1
>   then Leaf ()
>   else let k = n `div` 2
>        in Node (mkTree k) (mkTree (n-k))

> treeWord :: Tree a -> [a]
> treeWord (Leaf x) = [x]
> treeWord (Node x y) = treeWord x ++ treeWord y

> wordTree :: Tree b -> [a] -> Tree a
> wordTree (Leaf _) [x] = Leaf x
> wordTree (Node x y) xs =
>   Node (wordTree x (take (treeWidth x) xs))
>        (wordTree x (drop (treeWidth x) xs))

> treeWidth :: Tree a -> Nat
> treeWidth (Leaf _) = 1
> treeWidth (Node x y) = treeWidth x + treeWidth y

> sweep
>   :: (a -> d -> (b,u))
>   -> (d -> u -> u -> (u,d,d))
>   -> d
>   -> Tree a
>   -> (u, Tree b)

> sweep leaf node a (Leaf x) =
>   let (x',a') = leaf x a
>   in (a', Leaf x')
> 
> sweep leaf node a (Node x y) =
>   let (a',p',q') = node a p q
>       (p,x') = sweep leaf node p' x
>       (q,y') = sweep leaf node q' y
>   in (a', Node x' y')


> tscanr :: (a->a->a) -> a -> Tree a -> (a, Tree a)
> tscanr f a =
>   let leaf x a = (a,x)
>       node a p q = (f p q, f q a, a)
>   in sweep leaf node a

--------------------------------------------------------------------
 			   Tree Sweep Adder
---------------------------------------------------------------------

> add4 :: Signal a => a -> [(a,a)] -> (a,[a])
> add4 c zs =
>   let ps = map bcarrySym zs
>       ps' = wordTree (mkTree (length zs)) ps
>       (cf, cft) = tscanr composeSym P ps'
>       cfs = treeWord cft
>       cs = zipWith applySym cfs (repeat c)
>       c' = applySym cf c
>       ss = zipWith bsum zs cs
>   in (c', ss)

--------------------------------------------------------------------
 			Log Time Adder Circuit
---------------------------------------------------------------------

> type BSym a = (a,a)

> repK, repP, repG :: Signal a => BSym a
> repK = (zero,zero)
> repP = (zero,one)
> repG = (one,one)

> bcarryBSym :: Signal a => (a,a) -> BSym a
> bcarryBSym = id



> composeBSym :: Signal a => BSym a -> BSym a -> BSym a
> composeBSym f g =
>   let (g0,g1) = g
>   in  (mux2 f zero g0 g0 one,
>        mux2 f zero g1 g1 one)

> applyBSym :: Signal a => BSym a -> a -> a
> applyBSym f x = mux2 f zero x x one
> 
> add5 :: Signal a => a -> [(a,a)] -> (a,[a])
> add5 c zs =
>   let ps = map bcarryBSym zs
>       ps' = wordTree (mkTree (length zs)) ps
>       (cf, cft) = tscanr composeBSym repP ps'
>       cfs = treeWord cft
>       cs = zipWith applyBSym cfs (repeat c)
>       c' = applyBSym cf c
>       ss = zipWith bsum zs cs
>   in (c', ss)

--------------------------------------------------------------------
 			 Running the Examples
---------------------------------------------------------------------

> showBit :: Bit -> String
> showBit False = "0"
> showBit True  = "1"

> showWord :: [Bit] -> String
> showWord xs = concat (map showBit xs)

> testAdd
>   :: (Bit -> [(Bit,Bit)] -> (Bit,[Bit]))
>   -> Nat -> Nat -> Nat -> Nat -> IO ()
> testAdd circuit k x y cin =
>   do putStrLn ("Add " ++ show x  ++ " + " ++ show y
>                ++ " + " ++ show cin
>                ++ " (size " ++ show k ++ ")")
>      putStrLn ("   Correct result is " ++ show (x+y+cin))
>      let xs = represent k x
>      let ys = represent k y
>      let c = toBit cin
>      putStrLn ("   Circuit input is " ++ showWord xs
>                ++ " + " ++ showWord ys ++ " + " ++ showBit c)
>      let (c',ss) = circuit c (zip xs ys)
>      putStrLn ("   Circuit output is " ++ showBit c'
>                ++ " + " ++ showWord ss)
>      let res = bin ss + bit c'
>      putStrLn ("   Circuit output value is " ++ show res)

> main :: IO ()
> main =
>   do putStrLn "Running examples of the adder circuits"
> 
>      putStrLn "\nTesting add1..."
>      testAdd add1 8 3 9 0
>      testAdd add1 16 159 748 1
>      testAdd add1 16 3071 20584 0
> 
>      putStrLn "\nTesting add2..."
>      testAdd add2 8 3 9 0
>      testAdd add2 16 159 748 1
>      testAdd add2 16 3071 20584 0
> 
>      putStrLn "\nTesting add3..."
>      testAdd add3 8 3 9 0
>      testAdd add3 16 159 748 1
>      testAdd add3 16 3071 20584 0
> 
>      putStrLn "\nTesting add4..."
>      testAdd add4 8 3 9 0
>      testAdd add4 16 159 748 1
>      testAdd add4 16 3071 20584 0
> 
>      putStrLn "\nTesting add5..."
>      testAdd add5 8 3 9 0
>      testAdd add5 16 159 748 1
>      testAdd add5 16 3071 20584 0
