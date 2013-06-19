---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module Word4SeqTest where

> import Signal
> import SigBool
> import Format
> import BitWire
> import BitComb
> import BitSeq
> import Word4Seq

> main :: IO ()
> main =
>   do putStrLn "Word4SeqTest"
>      putStrLn "Testing sr4..."
>      sim_sr4 sr4
>      putStrLn "Testing sr4_v1..."
>      sim_sr4 sr4_v1
>      putStrLn "Testing sr4_v2..."
>      sim_sr4 sr4_v2
>      putStrLn "Testing sr4_v3..."
>      sim_sr4 sr4_v3


The 4-Bit Bidirectional Shift Register
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


> sim_sr4 sr_circuit =
>   let input =
> --      op  l  r  x        op    produce  state
> --      ~~~~~~~~~~~       ~~~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 0, 9],  -- load     1001    0000
>         [0, 0, 0, 3],  -- nop      1001    1001
>         [2, 1, 0, 4],  -- right 1  1100    1001
>         [2, 0, 0, 2],  -- right 0  0110    1100
>         [3, 0, 1, 7],  -- left  1  1101    0110
>         [1, 0, 0, 5],  -- load     0101    1101
>         [0, 0, 0, 0]]  -- nop      0101    0101
>       op = getbit2  input 0
>       l =  getbit   input 1
>       r =  getbit   input 2
>       x =  getbin 4 input 3
>       (op0,op1) = op
>       y = sr_circuit op l r x
>       spec :: [Format Bool]
>       spec = [string "Input: ",
>               bit op0, bit op1, string " ",
>               bit l, string " ", bit r, string " ", bits x,
>               string "    Output: ", bits y]
>   in run input spec


We can define the shift register circuit more generally with the
mscanr combinator; note that in this simulation, we use the general
circuit "sr" rather than the restricted 4-bit sr4.  The test data
is identical to that used above, and you can check that the circuit
defined in this more general way has the same behaviour on this
test data.

> {-  sim_gen_sr_4 =
>   let input =
> --      op  l  r  x        op    produce  state
> --      ~~~~~~~~~~~       ~~~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 0, 9],  -- load     1001    0000
>         [0, 0, 0, 3],  -- nop      1001    1001
>         [2, 1, 0, 4],  -- right 1  1100    1001
>         [2, 0, 0, 2],  -- right 0  0110    1100
>         [3, 0, 1, 7],  -- left  1  1101    0110
>         [1, 0, 0, 5],  -- load     0101    1101
>         [0, 0, 0, 0]]  -- nop      0101    0101
>       op = getbit2  input 0
>       l =  getbit   input 1
>       r =  getbit   input 2
>       x =  getbin 4 input 3
>       (op0,op1) = op
>       (p,q,y) = sr op l r x
>       spec :: [Format Bool]
>       spec = [string "Input: ",
>               bit op0, bit op1, string " ",
>               bit l, string " ", bit r, string " ", bits x,
>               string "    Output: ",
>               bit p, bit q, string " ", bits y]
>   in run input spec -}

Here is a similar example, but where the word size is increased to
8.  Notice that all we had to do to change the wordsize was to
change the number of bits in x; the general mscanr combinator
automatically accommodates itself to the new word size!

> {- sim_gen_sr_8 =
>   let input =
> --      op  l  r  x        op        state
> --      ~~~~~~~~~~~       ~~~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 0, 75],  -- load     0000 0000
>         [0, 0, 0,  3],  -- nop      0100 1011
>         [2, 1, 0,  4],  -- right 1  0100 1011
>         [2, 0, 0,  2],  -- right 0  1010 0101
>         [3, 0, 1,  7],  -- left  1  0101 0010
>         [1, 0, 0,  5],  -- load     1010 0101
>         [0, 0, 0,  0]]  -- nop      0000 0101
>       op = getbit2  input 0
>       l =  getbit   input 1
>       r =  getbit   input 2
>       x =  getbin 8 input 3  -- 8 bit words!
>       (op0,op1) = op
>       (p,q,y) = sr op l r x
>       spec :: [Format Bool]
>       spec = [string "Input: ",
>               bit op0, bit op1, string " ",
>               bit l, string " ", bit r, string " ", bits x,
>               string "    Output: ",
>               bit p, bit q, string " ", bits y]
>   in run input spec -}





