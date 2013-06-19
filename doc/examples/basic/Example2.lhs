----------------------------------------------------------------------
			University of Glasgow
		  Dip/MSc in Information Technology
			Core: Computer Systems
		  Tutorial, Week 7, 19 November 2001
----------------------------------------------------------------------

> module Tutorial where
> import Hydra
> import StdCircuit

---------------------------------------------------------------------------
Problem 1a.  Enter the following expression:

truthTable31 circuit1a

> circuit1a a b c = inv (xor2 (and2 a b) (or2 b c))

---------------------------------------------------------------------------
Problem 1b.  Enter the following expression:

truthTable31 circuit1b

> circuit1b x y z =
>   let a = and3 x y z
>       b = or2 y z
>   in (a, xor2 a b)

---------------------------------------------------------------------------
Problem 1c.  Enter the following expression:

sim1c input_1c

> circuit1c a =
>   let x = dff (xor2 x a)
>   in x

> input_1c :: [[Int]]
> input_1c =
>   [[1],
>    [0],
>    [1],
>    [1],
>    [0],
>    [1],
>    [1]]

> sim_1c input =
>   let a = getbit input 0
>       x = circuit1c a
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "  a=", bit a, string "  x=", bit x]
>   in do putStrLn "\nSimulating circuit1c"
>         run input simoutput

---------------------------------------------------------------------------
Problem 1d.  Enter the following expression:

sim_1d input_1d

> circuit1d a b =
>   let u = and2 a q
>       v = xor2 b p
>       p = dff u
>       q = dff v
>   in (p,q)

> input_1d :: [[Int]]
> input_1d =
>   [[0,0],
>    [1,1],
>    [1,0],
>    [1,1],
>    [1,1],
>    [0,1]]

> sim_1d input =
>   let a = getbit input 0
>       b = getbit input 1
>       (p,q) = circuit1d a b
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "  a=", bit a, string "  b=", bit b,
>          string "  p=", bit p, string "  q=", bit q]
>   in do putStrLn "\nSimulating circuit1d"
>         run input simoutput

---------------------------------------------------------------------------
Problem 5.  Enter the following expression:

truthTable31 circuit5

> circuit5 a b c =
>   or2 (and3 (inv a) (inv b) (inv c))
>       (and3 a (inv b) c)

---------------------------------------------------------------------------
Problem 6.  Enter the following expression:

sim_regfile_dec input_5a
sim_regfile_dec input_5b

> input_5a :: [[Int]]
> input_5a =
> -- ld  d  sa  sb   x
> -- ~~~~~~~~~~~~~~~~~~~
>   [[1, 3,  5,  3,  12],
>    [0, 0,  5,  3,  00],
>    [1, 6,  2,  6,  35]]

> input_5b :: [[Int]]
> input_5b =
> -- ld  d  sa  sb   x
> -- ~~~~~~~~~~~~~~~~~~~
>   [[1, 4,  0,  0,  29],
>    [1, 3,  0,  4,  11],
>    [0, 4,  4,  3,  13],
>    [1, 6,  6,  3,  21],
>    [1, 6,  6,  4,  15],
>    [0, 1,  6,  4,  98]]

> sim_regfile_dec input =
>   let k = 4  -- there are 2^4 = 16 registers
>       n = 16 -- each register contains 16 bits
>       ld = getbit   input 0
>       d  = getbin k input 1
>       sa = getbin k input 2
>       sb = getbin k input 3
>       x  = getbin n input 4
>       (a,b) = regfile n k ld d sa sb x
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, string " ", bindec 1 d, string " ",
>          bindec 1 sa, string " ", bindec 1 sb,
>          string " ", bindec 3 x,
>          string "   Output: ",
>          bindec 3 a, string " ", bindec 3 b]
>   in do putStrLn "\nSimulating register file"
>         run input simoutput
