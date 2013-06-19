---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Format where

This module supplies a wide variety of data formatting services,
including conversions between number systems, converting input strings
into internal signal representations, and converting signals to
readable strings for output.

> import Signal
> import SigStream
> import Group


---------------------------------------------------------------------------
		      Number System Conversions
---------------------------------------------------------------------------

> hexbin :: Int -> String -> Int
> hexbin k cs =
>   let k' = k `div` 4 
>       i = sum [hexDigVal c * 16^(k'-(i+1)) | (c,i) <- zip cs [0..]]
>   in i
> 
> hextc :: Int -> String -> Int
> hextc k cs =
>   let i = sum [hexDigVal c * 16^(k-(i+1)) | (c,i) <- zip cs [0..]]
>   in if hexDigVal (cs!!0) >=8
>     then i - 2^(4*k)
>     else i
> 
> hexDigVal :: Char -> Int
> hexDigVal '0' =  0
> hexDigVal '1' =  1
> hexDigVal '2' =  2
> hexDigVal '3' =  3
> hexDigVal '4' =  4
> hexDigVal '5' =  5
> hexDigVal '6' =  6
> hexDigVal '7' =  7
> hexDigVal '8' =  8
> hexDigVal '9' =  9
> hexDigVal 'a' = 10
> hexDigVal 'b' = 11
> hexDigVal 'c' = 12
> hexDigVal 'd' = 13
> hexDigVal 'e' = 14
> hexDigVal 'f' = 15

> hexDig :: [Int] -> Char
> hexDig bs =
>   let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
>   in "0123456789abcdef" !! i

> {- ?? todo - remove this
> hexDig' :: [Int] -> String
> hexDig' bs =
>   let i = sum [b * 2^(4-(i+1)) | (b,i) <- zip bs [0..]]
>   in  show i --"0123456789abcdef" !! i
> -}

Let xs be a k-bit word whose binary interpretation is n.  Then tc k n
gives the two's complement representation of the word.  For example,

  tc 8 5 = 5    (because 0000 0101 is nonnegative)
  tc 8 255 = -1 (because 1111 1111 is negative and represents -1)

> tc :: Int -> Int -> Int
> tc k n =
>   if n >= 2^(k-1)
>     then n - 2^k
>     else n


---------------------------------------------------------------------------
		       Simulation Driver Input
---------------------------------------------------------------------------

The input section of a simulation driver is constructed using getbit,
getbin, gettc, and ...

Extract input signals from integer lists
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 
> getbit :: (Signal a, Static a) => [[Int]] -> Int -> Stream a
> getbit xs i =
>   Cycle (intSig (head xs !! i)) (getbit (tail xs) i)

> getbit2
>   :: (Signal a, Static a)
>   => [[Int]] -> Int -> (Stream a, Stream a)
> getbit2 xs i = (getbini 2 0 xs i, getbini 2 1 xs i)


> getbit20, getbit21
>   :: (Signal a, Static a)
>   => [[Int]] -> Int -> Stream a
> getbit20 xs i =
>   Cycle (bit20 (head xs !! i)) (getbit20 (tail xs) i)
> getbit21 xs i =
>   Cycle (bit21 (head xs !! i)) (getbit20 (tail xs) i)

> bit20, bit21 :: (Signal a, Static a) => Int -> a
> bit20 x = intSig ((x `mod` 4) `div` 2)
> bit21 x = intSig (x `div` 2)

Makes a stream consisting of the j'th bit of the k-bit binary
representation of the i'th column of the list xs.

> getbini k j xs i =
>   Cycle (biti k j (head xs !! i)) (getbini k j (tail xs) i)

Builds a k-bit word (list of streams) which is the binary
representation of the i'th column of xs::[[Int]].

> getbin k xs i = [getbini k j xs i | j <- [0..k-1]]

> gettc k xs i = [getbini k j (map (map (tc k)) xs) i | j <- [0..k-1]]

The biti function gives the i'th bit of the k-bit binary
representation of the integer x. The wordsize is k, i is the bit
index, x is the integer input Bits are numbered from the left
starting with 0, thus bit 0 is the most significant bit.

> biti :: (Signal a, Static a) => Int -> Int -> Int -> a
> biti k i x = boolSig (odd (x `div` (2^(k-(i+1)))))


---------------------------------------------------------------------------
		       Simulation Driver Output
---------------------------------------------------------------------------

The output section of a simulation driver uses format specifiers to
control how various signals are printed.  These format specifiers are
string, bit, bindec, binhex, tcdec.

User functions for constructing format specifications
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> bit x = FmtBit fmtBit [] x
> bits x = FmtWord fmtWord [] n n x
>   where n = length x
> string x = FmtString x

> bindec w x = FmtWord fmtBin [] w (length x) x

> binhex x = FmtHex fmtHex (length x) x  -- deprecated, use hex instead
> hex x = FmtHex fmtHex (length x) x
> bitstc w x = FmtWord fmtTC  [] w (length x) x  -- deprecated, use tcdec

> tcdec w x = FmtWord fmtTC  [] w (length x) x


Representation of output format specification
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> data Format a
>   = FmtBit (String -> Stream a -> (String, Stream a))
>       String (Stream a)
>   | FmtWord
>       (String -> Int -> Int -> [Stream a]
>          -> (String, [Stream a]))
>       String Int Int [Stream a]
>   | FmtString String
>   | FmtHex (Int -> [Stream a] -> (String, [Stream a])) Int [Stream a]


Convert current cycle value to string
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The fmtBit function converts a bit signal for output.  The argument x
is a bit signal, and a character is output each clock cycle giving the
value of the signal.

?? todo remove label

> fmtBit :: (Signal a, Static a) => String -> Stream a -> (String, Stream a)
> fmtBit lbl x = (showSig (current x), future x)

A word signal (a list of bit signals) can be formatted for output in a
variety of ways.  To display it as a sequence of bits, specify the
format with fmtWord.

> fmtWord
>  :: (Signal a, Static a)
>   => String -> Int -> Int -> [Stream a] -> (String, [Stream a])
> fmtWord lbl w k x =
>   let z = setlength w (concat (map (showSig . current) x))
>   in (z, map future x)

A word signal can be converted to a decimal integer for output.  If
the word is binary, use the fmtBin function, but if it's two's
complement, use fmtTC.

> fmtBin, fmtTC
>   :: (Signal a, Static a)
>   => String -> Int -> Int -> [Stream a] -> (String, [Stream a])

> fmtBin lbl w k x =
>   let z = map (sigInt . current) x
>       n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
>   in (setlength w (show n), map future x)

> fmtTC lbl w k x =
>   let z = map (sigInt . current) x
>       n = sum [b * 2^(k-(i+1)) | (b,i) <- zip z [0..]]
>       m = if head z == 0
>           then n
>           else n - 2^k
>   in (setlength w (show m), map future x)

A sequential word signal x can be converted to k-digit hexadecimal
using fmtHex k x.

> fmtHex :: Static a => Int -> [Stream a] -> (String, [Stream a])
> fmtHex k x =
>   let z = map (sigInt . current) x
>       j = k `mod` 4
>       z' = take j (repeat 0) ++ z
>       s = [hexDig (field z' i 4) | i <- [0,4..k-1]]
>   in (s, map future x)
> 


Produce output cycle by cycle
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> run inps outs = run' (length inps) 0 outs

> run' :: (Signal a, Static a) => Int -> Int -> [Format a] -> IO ()
> run' limit i xs =
>  if i<limit
>  then do xs' <- step i xs
>          run' limit (i+1) xs'
>  else return ()

> step :: (Signal a, Static a) => Int -> [Format a] -> IO [Format a]
> step i xs =
>   do putStr (setlength 4 (show i))
>      putStr ".  "
>      let ys = map dofmt xs
>      let s = concat (map fst ys)
>      putStr s
>      putStr "\n"
>      let xs' = map snd ys
>      return xs'

> dofmt :: (Signal a, Static a) => Format a -> (String, Format a)
> dofmt (FmtBit f lbl x) =
>   let (s,x') = f lbl x
>   in (s, FmtBit f lbl x')
> dofmt (FmtWord f lbl w k x) =
>   let (s,x') = f lbl w k x
>   in (s, FmtWord f lbl w k x')
> dofmt (FmtHex f k x) =
>   let (s,x') = f k x
>   in (s, FmtHex f k x')
> dofmt (FmtString s) = (s, FmtString s)


String Formatting
~~~~~~~~~~~~~~~~~

Text field adjustment

> setlength :: Int -> String -> String
> setlength w xs =
>   let n = length xs
>   in take (w-n) (repeat ' ') ++ xs

> center :: Int -> String -> String
> center i s =
>   let k = length s
>       j = (i - length s) `div` 2
>   in constring ' ' j ++ s ++ constring ' ' (i-k-j)

> constring :: Char -> Int -> String
> constring c i = take i (repeat c)

> indent :: Int -> String
> indent k = take (2*k) (repeat ' ')

> separatorLine :: IO ()
> separatorLine = putStrLn (take 60 (repeat '-'))


Miscellaneous
~~~~~~~~~~~~~

> justs :: [Maybe x] -> [x]
> justs [] = []
> justs (Nothing : xs) = justs xs
> justs (Just x : xs) = x : justs xs
