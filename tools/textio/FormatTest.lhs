---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module FormatTest where

> import Signal
> import SigBool
> import SigStream
> import Group
> import Format

> main :: IO ()
> main =
>   do standalone_driver test_input


> test_input :: [[Int]]
> test_input =
> --  x    y     z
> -- bit  bin   tc
> -- ~~~~~~~~~~~~~~~
>   [[0,    5,    5],     -- cycle 0
>    [1,  255,   -1],     -- cycle 1
>    [1,    0,  127],     -- cycle 2
>    [0,   42, -128],     -- cycle 3
>    [1,  195,  -37]      -- cycle 4
>   ]

> standalone_driver :: [[Int]] -> IO ()
> standalone_driver input = do run input simoutput
>   where
>
> -- Size parameters
>     n = 8
>
> -- Input formatting
>     x = getbit   input 0 :: Stream Bool
>     y = getbin n input 1
>     z = gettc  n input 2
>
> -- Output formatting
>     simoutput :: [Format Bool]
>     simoutput =
>       [string "x = ", bit x,
>        string "      y = ", binhex  y, bindec 5 y, tcdec 5 y,
>        string "      z = ", binhex  z, string " ", bits z,
>          bindec 5 z, tcdec 5 z]


