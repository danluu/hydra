---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module WordSeqTest where

This module contains examples and test cases for the circuits defined
in WordSeq.

> import Signal
> import Group
> import SigBool
> import SigStream
> import Format
> import WordComb
> import WordWire
> import WordSeq

> main =
>   do putStrLn "Register file..."
>      sim_regfile
>      putStrLn "Register file, decimal output"
>      sim_regfile_dec
>      putStrLn "Register Transfer Machine"
>      sim_rtm
>      putStrLn "Multiplier"
>      sim_mult

	
Simulating the Register File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> sim_regfile =
>   let k = 3  -- there are 2^3 = 8 registers
>       n = 8  -- each register contains 8 bits
>       input =
> --      ld  d sa sb  x
> --      ~~~~~~~~~~~~~~~
>        [[1, 4, 0, 0,  25],  -- R4 :=  25    R0 =   0, R0 =   0
>         [1, 7, 4, 7, 255],  -- R7 := 255    R4 =  25, R7 =   0
>         [1, 1, 4, 7,  31],  -- R1 :=  31    R4 =  25, R7 = 255
>         [0, 1, 0, 1,  50],  --              R0 =   0, R1 =  31
>         [1, 2, 1, 7, 100],  -- R2 := 100,   R1 =  31, R7 = 255
>         [0, 0, 0, 2,   0]]  --              R0 =   0  R2 = 100
>       ld = getbit   input 0
>       d  = getbin k input 1
>       sa = getbin k input 2
>       sb = getbin k input 3
>       x  = getbin n input 4
>       (a,b) = regfile n k ld d sa sb x
>       spec :: [Format Bool]
>       spec =
>         [string "Input: ",
>          bit ld, string " ", bits d, string " ",
>          bits sa, string " ", bits sb, string " ", bits x,
>          string "   Output: ", bits a, string " ", bits b]
>   in run input spec

The following is the same as sim_regfile, but it prints the output
using decimal representations.  The only difference appears in
spec, where bindec is used instead of bits for printing the binary
numbers in decimal notation.

> sim_regfile_dec =
>   let k = 3  -- there are 2^3 = 8 registers
>       n = 8  -- each register contains 8 bits
>       input =
> --      ld  d sa sb  x
> --      ~~~~~~~~~~~~~~~
>        [[1, 4, 0, 0,  25],  -- R4 :=  25    R0 =   0, R0 =   0
>         [1, 7, 4, 7, 255],  -- R7 := 255    R4 =  25, R7 =   0
>         [1, 1, 4, 7,  31],  -- R1 :=  31    R4 =  25, R7 = 255
>         [0, 1, 0, 1,  50],  --              R0 =   0, R1 =  31
>         [1, 2, 1, 7, 100],  -- R2 := 100,   R1 =  31, R7 = 255
>         [0, 0, 0, 2,   0]]  --              R0 =   0  R2 = 100
>       ld = getbit   input 0
>       d  = getbin k input 1
>       sa = getbin k input 2
>       sb = getbin k input 3
>       x  = getbin n input 4
>       (a,b) = regfile n k ld d sa sb x
>       spec :: [Format Bool]
>       spec =
>         [string "Input: ",
>          bit ld, string " ", bindec 1 d, string " ",
>          bindec 1 sa, string " ", bindec 1 sb,
>          string " ", bindec 3 x,
>          string "   Output: ",
>          bindec 3 a, string " ", bindec 3 b]
>   in run input spec


Simulating the Register Transfer Machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> sim_rtm =
>   let n = 8  -- can also try 30
>       k = 3  -- can try 5
>       input =
> --      ld add d sa sb   x
> --      ~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 3, 0, 0, 125], -- R3 :=   x   = 125. R0=  0 R0=  0
>         [1, 0, 6, 3, 0,  10], -- R6 :=   x   =  10. R3=125 R0=  0
>         [1, 1, 2, 3, 6,   0], -- R2 := R3+R6 = 135. R3=125 R6= 10
>         [1, 0, 1, 1, 2,  75], -- R1 :=   x   =  75. R1=  0 R2=135
>         [1, 1, 1, 1, 2,   0], -- R1 := R1+R2 = 210. R1= 75 R2=135
>         [0, 0, 0, 1, 2,   0]] -- nop                R1=210 R2=135
>       ld  = getbit   input 0
>       add = getbit   input 1
>       d   = getbin k input 2
>       sa  = getbin k input 3
>       sb  = getbin k input 4
>       x   = getbin n input 5
>       (a,b,y,c,s) = rtm n k ld add d sa sb x
>       spec :: [Format Bool]
>       spec =
>         [string "Input: ",
>          bit ld, bit add, bindec 2 d, bindec 2 sa, bindec 2 sb,
>          bindec 4 x,
>          string "  Output: ",
>          bindec 4 a, bindec 4 b, bindec 4 y, bindec 4 s]
>   in run input spec


Simulating the Sequential Multiplier
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~



> sim_mult =
>   let k = 8
>       input =
> --     start  x    y
> --     ~~~~~~~~~~~~~~
>        [[1,  50,  75],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [1, 100, 100],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [1, 100, 100],
>         [0,   0,   0],
>         [0,   0,   0],
>         [1,   2,   3],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0],
>         [0,   0,   0]]
>       start = getbit   input 0
>       x     = getbin k input 1
>       y     = getbin k input 2
>       (rdy,prod,rx,ry,s) = mult k start x y
>       spec :: [Format Bool]
>       spec =
>         [string "Input: ",
>          bit start, bindec 4 x, bindec 4 y,
>          string "  Output: ",
>          bit rdy, bindec 6 prod, bindec 4 rx, bindec 6 ry,
>          bindec 6 s]
>     in run input spec
