




I/O Operations
~~~~~~~~~~~~~~

> doList :: (a->String) -> [a] -> IO ()
> doList f [] = return ()
> doList f (x:xs) =
>   do putStrLn (f x)
>      doList f xs

> doall :: (a -> IO ()) -> [a] -> IO ()
> doall f [] = return ()
> doall f (x:xs) =
>   do f x
>      doall f xs


		     IOS: Input/Output with state
                     ~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> newtype IOS s a = IOS (s -> IO (a, s))

> unIOS :: IOS s a -> (s -> IO (a, s))
> unIOS (IOS m) = m

> liftIOS :: IO a -> IOS s a
> liftIOS m = IOS (\s -> m >>= \a -> return (a,s))

> runIOS :: IOS s a -> s -> IO (a, s)
> getIOS :: IOS s s
> setIOS :: s -> IOS s ()
> modIOS :: (s -> s) -> IOS s s

> runIOS        = unIOS 
> modIOS f      = IOS (\s -> return (s, f s))
> setIOS s'     = IOS (\s -> return ((),s'))
> getIOS        = IOS (\s -> return (s,s))

> {-# INLINE liftIOS #-}
> {-# INLINE runIOS #-}
> {-# INLINE modIOS #-}
> {-# INLINE setIOS #-}
> {-# INLINE getIOS #-}

> thenIOS m k= IOS (\s -> unIOS m s >>= \(a,s') -> unIOS (k a) s')
> then_IOS m k= IOS (\s -> unIOS m s >>= \(_,s') -> unIOS k s')
> retIOS a = IOS (\s -> return (a,s))

> {-# INLINE thenIOS #-}
> {-# INLINE then_IOS #-}
> {-# INLINE retIOS #-}


warning problem comes from the following

> instance Monad (IOS s) where
>   (>>=)  = thenIOS
>   (>>)   = then_IOS
>   return = retIOS



 -- instance Functor (IOS s) where map f xs = [ f x | x <- xs ]



 -- left inverse of IOS (not exported)
 {-# INLINE unIOS #-}


-------------------------------------------------------------------
-- Basic operations for data parallel monad

> rundp :: IOS s a -> s -> IO ()
> rundp prog initst =
>   do runIOS prog initst
>      return ()

> dp_putStr :: String -> IOS s ()
> dp_putStr xs = liftIOS (putStr xs)

> dp_getChar :: IOS s Char
> dp_getChar = liftIOS getChar

> dp_showState :: Show s => String -> IOS s ()
> dp_showState cs =
>   do dp_putStr cs
>      x <- getIOS
>      dp_putStr (show x)
>      dp_putStr "\n"
>      return ()

> dp_showStatenew :: (s -> IO ()) -> IOS s ()
> dp_showStatenew f =
>   do x <- getIOS
>      liftIOS (f x)
>      dp_putStr "\n"
>      return ()

> repeatLoop f =
>   do q <- f
>      if q then repeatLoop f
>           else return ()

-------------------------------------------------------------------
-- Data parallel monadic operations with list state

> dpl_map :: (a->a) -> IOS [a] [a]
> dpl_map f = modIOS (map f)

> dpl_foldl :: (a->b) -> (b->b->b) -> b -> IOS [a] b
> dpl_foldl f g a =
>   do xs <- getIOS
>      return (foldl g a (map f xs))

-------------------------------------------------------------------
-- Examples of monadic data parallel programs

-- Basic I/O with dp operations in the IOS monad

> dp_echoChar :: IOS [Int] ()
> dp_echoChar =
>   do dp_putStr "This is dp_echoChar, type in a character for me!\n"
>      c <- dp_getChar
>      dp_putStr ("\nyou typed in the character ---" ++ [c] ++ "---\n")
>      dp_showState "this is the initial state:\n  "
>      dp_putStr "bye\n"
>      return ()

> run_echoChar = rundp dp_echoChar [1..10]

-- Monadic data parallel maps, folds and scans with list state

> dp_alg :: IOS [Int] ()
> dp_alg =
>   do dp_putStr "\nThis is dp_alg\n"
>      dp_showState "initial state:\n  "
>      dpl_map (*10)
>      dp_showState "after mapping (*10):\n  "
>      x <- dpl_foldl id (+) 0
>      dp_putStr ("dplfold id (+) ==> " ++ show x ++ "\n")
>      dpl_map (+x)
>      dp_showState "after mapping (+x):\n  "
>      dp_putStr "bye\n\n"
>      return ()

> run_dp_alg = rundp dp_alg [1..10]

-------------------------------------------------------------------
-- Basic data parallel monad (without I/O)

> {-
> data DP s a = DP (s -> (a,s))

> runDP :: DP s a -> s -> (a,s)
> runDP = unwrapDP

--instance Functor (DP s) where
--  map f xs = [f x | x <- xs]

> instance Monad (DP s) where
>   (f >>= g) =
>     DP (\s -> case unwrapDP f s of
>                 (a,s') -> unwrapDP (g a) s')
>   return a = DP (\s -> (a,s))

> unwrapDP :: DP s a -> (s ->(a,s))
> unwrapDP (DP f) = f
> -}


this should go in HydraCLI, or in general functions...

> {-
> data Command
>   = Field Int
>   | ShowSig
>   | Up
>   | Quit
>   deriving (Read,Show)
> -}

> {-
> doCommand :: Command -> [Context a d] -> IO [Context a d]
> doCommand cmd xs =
>   case cmd of
>     Quit -> return []
>     Field i ->
>       do putStr "Field command\n"
>          case (head xs) of
>            Ctx_Clu c ->
>              do putStr "indexing cluster\n"
>                 putStr (showClu c ++ "\n")
>                 return (getClusterField c i : xs)
>            otherwise ->
>              do putStr "doing nothing\n"
>                 return xs
>     ShowSig ->
>       do putStr "ShowSig command\n"
>          case (head xs) of
>            Ctx_Sig s ->
>              putStr "Found signal\n"
>            otherwise ->
>              return ()
>          return xs
>     Up ->
>       do putStr "Up command\n"
>          return (tail xs)
> -}

Parse the string typed by the user in order to obtain the command
to be executed.  Not currently using these; for the time being,
will just just the derived Read operation.

> {-
> parseCommand :: String -> Command
> parseCommand ('C' : _) = Field 0
> parseCommand ('S' : _) = ShowSig
> -}

Remove leading comma from a string, if there is one.

> {-
> dropcomma :: String -> String
> dropcomma [] = []
> dropcomma (c:cs) =
>   if c==',' then cs else c:cs
> -}

