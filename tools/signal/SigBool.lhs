---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module SigBool where
> import Signal

> -- type Bit = Bool

Bool Signals
~~~~~~~~~~~~

> instance Signal Bool where
>   zero = False
>   one = True

>   buf = id
>   inv = not

>   and2 = (&&)
>   and3 a b c = a && b && c
>   and4 a b c d = a && b && c && d

>   or2 = (||)
>   or3 a b c = a || b || c
>   or4 a b c d  = a || b || c || d

>   xor2 = xor2Bool
>   xor3 a b c = xor2 a (xor2 b c)
>   xor4 a b c d = xor2 (xor2 a b) (xor2 c d)

>   xnor2 a b = not (xor2Bool a b)
>   xnor3 a b c = not (xor3Bool a b c)
>   xnor4 a b c d = not (xor4Bool a b c d)

>   nand2 a b = not (a && b)
>   nand3 a b c = not (and3 a b c)
>   nand4 a b c d  = not (and4 a b c d)

>   nor2 a b = not (a || b)
>   nor3 a b c = not (or3 a b c)
>   nor4 a b c d = not (or4 a b c d)

>   alias x _ _ _ = x

> xor2Bool :: Bool -> Bool -> Bool
> xor2Bool False x = x
> xor2Bool True  x = not x

> xor3Bool :: Bool -> Bool -> Bool -> Bool
> xor3Bool a b c = xor2Bool a (xor2Bool b c)

> xor4Bool :: Bool -> Bool -> Bool -> Bool -> Bool
> xor4Bool a b c d = xor2Bool (xor2Bool a b) (xor2Bool c d)


Static Bool Signals
~~~~~~~~~~~~~~~~~~~

> instance Static Bool where
>   is0 = not
>   is1 = id
>   boolSig = id
>   sigBool = id
>   intSig  = intSigBool
>   sigInt  = sigIntBool
>   showSigChar = showSigBool
>   showSig x = [showSigBool x]
>   readSigChar = undefined

> intSigBool :: Int -> Bool
> intSigBool x
>   | x==0 = False
>   | otherwise = True

> sigIntBool :: Bool -> Int
> sigIntBool False = 0
> sigIntBool True = 1

> showSigBool False = '0'
> showSigBool True  = '1'


Structured Bool Signals
~~~~~~~~~~~~~~~~~~~~~~~

junk...???

> box11Bool
>   :: String -> String -> String -> b
>   -> (Bool->Bool)
>   -> Bool -> Bool
> box11Bool _ _ _ _ f = f

> box21Bool
>   :: String -> String -> String -> String
>   -> (Bool->Bool->Bool)
>   -> Bool -> Bool -> Bool
> box21Bool _ _ _ _ f = f

> box31Bool
>   :: String -> String -> String -> String -> String
>   -> (Bool->Bool->Bool->Bool)
>   -> Bool -> Bool -> Bool -> Bool  
> box31Bool _ _ _ _ _ f = f

> box41Bool
>   :: String -> String -> String -> String -> String -> String
>   -> (Bool->Bool->Bool->Bool->Bool)
>   -> Bool -> Bool -> Bool -> Bool -> Bool
> box41Bool _ _ _ _ _ _ f = f

> box22Bool
>   :: String -> String -> String -> String -> (String,String)
>   -> (Bool->Bool->(Bool,Bool))
>   -> Bool -> Bool -> (Bool,Bool)
> box22Bool _ _ _ _ _ f = f


Junk.....

> {-
>   is0     = not
>   is1     = id
>   box11 = box11Bool
>   box21 = box21Bool
>   box31 = box31Bool
>   box41 = box41Bool
>   box22 = box22Bool
> -}

> {-
> mkOutBool
>   :: String -> [CluSpec Bool] -> CluSpec Bool -> Cluster Bool

> mkOutBool _ xs y = mkCluster TopCluster y
> {-   let box = Box
>         {boxSpecies = boxn,
>          boxTag = undefined,
>          boxChildren = [],  -- todo
>          boxInPorts = inps,
>          boxInternal = [],  -- todo
>          boxOutPorts = out}
>       inps = zipWith g xs [0..]
>       g x i = mkCluster TopCluster x
>       out = mkCluster TopCluster y -}
> -}
