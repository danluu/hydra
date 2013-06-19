----------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
	      Tutorial 2: The Register Transfer Machine
			    John O'Donnell
----------------------------------------------------------------------

The subject of this tutorial is simulation drivers: software tools
that make it easier to write down the inputs and to read and
understand the outputs when you're simulating a circuit.

> module Tutorial2 where

> import Hydra
> import CircuitLib
> import Word4

We will be using some circuits that operate on 4-bit words, as well as
the basic circuit library.

---------------------------------------------------------------------------
Simulation Drivers
---------------------------------------------------------------------------

Small circuits can be simulated by providing the values of all the
individual input bits, and reading the resulting output bits.  For
larger scale designs, there are just too many bits for this to be
feasible.  To simulate a processor, there are likely to be several
input words and a dozen of more output words; this would require the
user to read through hundreds of 0s and 1s, and to do that again and
again for each clock cycle.

A "simulation driver" is a piece of software that takes input in a
readable form, runs the simulation, and formats the output signals.
We will use a simulation driver for each of our main circuits. 
We'll provide inputs to sequential circuits by making a list, where
each line of the list corresponds to a clock cycle, and it contains
the input signal values for that cycle (expressed in decimal).

Since any clocked signal is a signal, there is no problem with doing a
synchronous simulation of a combinational circuit.


The simulation driver for a synchronous circuit contains four main
pieces:

(1) choices for the circuit parameters, if any; these typically
include word size and address size;

(2) an equation that applies the circuit to its inputs, defining names
for the outputs;

(3) tools that take the inputs expressed in a form easy for the user
to write and convert them to the proper input signal representation;
and

(4) tools that take the output signals from the circuit and format
them readably.


Using a Simulation Driver for the Ripple Carry Adder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

To show how to write a simulation driver, this section will work
through an example in detail: a driver for the ripple carry adder,
with type

   Clocked a => a -> [(a,a)] -> (a,[a])

The adder has one circuit parameter, its wordsize, since our ripple
carry adders may be defined using design patterns that work for
arbitrary size!  Here we use 8-bit words.

   n = 8

The heart of the simulation driver is an equation that uses the
circuit.  Here, we define the carry output co and the sum word s to be
the outputs of an adder with input words x and y; these are
represented in bit slice form as a single word z :: [(a,a)].

    (co,s) = rippleAdd ci z

The bit slice word z is formed by the bitslice2 wiring pattern:
    z = bitslice2 x y
We could also omit this equation, and write the circuit application as
    (co,s) = rippleAdd ci (bitslice2 x y)

The inputs to the circuit can be provided interactively, but sometimes
it's more convenient to define a set of inputs as a constant
definition in the simulation module.  This allows us to run a
simulation repeatedly, without having to keep typing the same inputs
over and over again.  That approach will be taken here; later we will
introduce tools that support interactive simulations.

There are three values to be supplied for each clock cycle: the carry
input, and the two words x and y.  These will be written in a list of
lists; here's an example definition of test input:

> add_input1 :: [[Int]]

> add_input1 =
>   [[0,  2,  3],
>    [0,  1,  8],
>    [0, 10,  5],
>    [0, 11,  2],
>    [1,  4,  5]]

This says that in clock cycle 0, the carry input is 0, x is 3, and y
is 8; in clock cycle 1 x=5 and y=-9, and so on.  Note that the words
can be written in decimal in the test data.

The simulation driver is called sim_adder, and you give it three arguments:
  -- the name of the adder circuit you want to use
  -- the wordsize
  -- the input data
For example,

   sim_adder rippleAdd4  4 add_input1

says to use the 4-bit ripple carry adder (the same one presented in
lecture) with the data above.  Go ahead and enter that line at the
prompt, and you should see output something like this...

Tutorial1> sim_adder rippleAdd4 4 add_input1

..................................................
   Simulating ripple carry adder
      0.   ci=0 x=   2 y=   3   Output: 0   5
      1.   ci=0 x=   1 y=   8   Output: 0   9
      2.   ci=0 x=  10 y=   5   Output: 0  15
      3.   ci=0 x=  11 y=   2   Output: 0  13
      4.   ci=1 x=   4 y=   5   Output: 0  10
..................................................


Now you can try some other experiments, for example by using the
general n-bit adder (rippleAdd) or by varying the wordsize.  Of course
you can also modify the input test data.  To run the experiments,
enter the following expressions:

   sim_adder rippleAdd4  4 add_input1
   sim_adder rippleAdd   4 add_input1
   sim_adder rippleAdd  16 add_input1


Writing a Simulation Driver for the Ripple Carry Adder
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we need to use the tools for converting the test input to the
correct signal representations:

   ci = getbit   input 0 :: Stream Bool 
   x  = getbin n input 1
   y  = getbin n input 2

The first equation says that ci is obtained from the 0'th column of
the test input, using a bit conversion (getbit).  The second equation
says that x comes from column 1 in the test input, and it's converted
using a binary conversion (getbin n), where n is the wordsize.  The
last equation converts y from column 2 of the input data.

The final step is to format the output.  First, we specify the
underlying signal representation that is being used:

   simoutput :: [Format Bool]

This is necessary, because Hydra supports a large number of signal
representations, and it needs to know which one to use here.

Now we define the simulation output by formatting the various signals
that should be printed.  The format consists of a list of fields
separated by commas.  For every clock cycle, Hydra will print a line
comprising all these fields.  The field format specifications are

  string "abc"  prints the literal string on each line
  bit x         prints the value of the bit signal x, as 0 or 1
  bindec k x    converts the binary word x to a 4-digit decimal integer
  tcdec k x     converts the two's complement word x to a 4-digit
                decimal integer

The following format prints the adder's inputs and outputs, along with
some labels:

    simoutput =
      [bit ci,
       string " x= ", bindec 4 x, tcdec 4 x,
       string " y= ", bindec 4 y, tcdec 4 y,
       string " Output: ", bit co,
       string " sum= ", bindec 4 s, tcdec 4 s]

Putting all the pieces together, here is the simulation driver for the
ripple carry adder:

> sim_adder add_circuit n input =
>   let (co,s) = add_circuit ci (bitslice2 x y)
>       ci = getbit   input 0
>       x  = getbin n input 1
>       y  = getbin n input 2
>       simoutput :: [Format Bool]
>       simoutput =
>         [string " ci=", bit ci,
>          string " x= ", bindec 3 x,
>          string " y= ", bindec 3 y,
>          string "   Output: ", bit co, bindec 4 s]
>   in do putStrLn "\nSimulating ripple carry adder"
>         run input simoutput




A simulation driver for the comparitor
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The ripple comparitor for binary numbers can be simulated by entering
the following:

   sim_comparitor rippleCmp 16 cmp_input1

Here is some test data for the comparitor...

> cmp_input1 :: [[Int]]
> cmp_input1 =
>   [[2, 3],
>    [3, 2],
>    [3, 3],
>    [1, 8],
>    [8, 1],
>    [9, 9],
>    [0, 5],
>    [7, 5]]

The simulation driver is similar to the adder driver.

> sim_comparitor circuit n input =
>   let (lt,eq,gt) = circuit (bitslice2 x y)
>       x = getbin n input 0
>       y = getbin n input 1
>       simoutput :: [Format Bool]
>       simoutput =
>         [string " x= ", bindec 4 x,
>          string "   y= ", bindec 4 y,
>          string "  (lt,eq,gt) = ",
>          bit lt, bit eq, bit gt]
>   in do putStrLn "\nSimulating comparitor"
>         run input simoutput





The Bidirectional Shift Register
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

The bidirectional shift register takes an operation code that
determines the behavior:
  0 -- no state change
  1 -- load input word x
  2 -- shift right
  3 -- shift left

The following test data contains comments showing the expected output.
This is a useful technique for documenting a circuit, and it provides
a good permanent test case.  You can run the simulation by copying the
following expression, and pasting it into a Hydra dialogue:

  sim_sr4 sr4_testdata_1


> sr4_testdata_1 :: [[Int]]
> sr4_testdata_1 =
> --      op  l  r  x        op    produce  state
> --      ~~~~~~~~~~~       ~~~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 0, 9],  -- load     1001    0000
>         [0, 0, 0, 3],  -- nop      1001    1001
>         [2, 1, 0, 4],  -- right 1  1100    1001
>         [2, 0, 0, 2],  -- right 0  0110    1100
>         [3, 0, 1, 7],  -- left  1  1101    0110
>         [1, 0, 0, 5],  -- load     0101    1101
>         [0, 0, 0, 0]]  -- nop      0101    0101

Here is a simulation driver intended specifically for the 4-bit
version of the circuit.

> sim_sr4 input =
>   let op = getbit2  input 0
>       l =  getbit   input 1
>       r =  getbit   input 2
>       x =  getbin 4 input 3
>       (op0,op1) = op
>       y = sr4 op l r x
>       simoutput :: [Format Bool]
>       simoutput = [string "Input: ",
>               bit op0, bit op1, string " ",
>               bit l, string " ", bit r, string " ", bits x,
>               string "    Output: ", bits y]
>   in do putStrLn "\nSimulate 4-bit shift register"
>         run input simoutput


Simulating the Register File
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Now we'll simulate a register file with 16 registers, each containing
16 bits.  This is the circuit used within the ITM!  The circuit
definition appears in WordSeq.lhs, and here is the test data:

> regfile_input1 :: [[Int]]
> regfile_input1 =
> --      ld  d sa sb  x
> --      ~~~~~~~~~~~~~~~
>        [[1, 4, 0, 0,  25],  -- R4 :=  25    R0 =   0, R0 =   0
>         [1, 7, 4, 7, 255],  -- R7 := 255    R4 =  25, R7 =   0
>         [1, 1, 4, 7,  31],  -- R1 :=  31    R4 =  25, R7 = 255
>         [0, 1, 0, 1,  50],  --              R0 =   0, R1 =  31
>         [1, 2, 1, 7, 100],  -- R2 := 100,   R1 =  31, R7 = 255
>         [0, 0, 0, 2,   0]]  --              R0 =   0  R2 = 100

This means, for example, that in the initial clock cycle ld=1, so x
(which is 25) will be loaded into reg[4] (because the destination
address d is 4).  On the next clock cycle, the source address sa is 4,
so the circuit will output the contents of reg[4], which by then will
be 25.  You should work through this input data in detail.

Run the simulation by entering

  sim_regfile regfile_input1

You can see that the long binary numbers are hard to read.  There is
another simulation driver which converts the numbers to decimal;
exactly the same circuit is simulated, but the output is more
readable.  You can try it by entering

  sim_regfile_dec regfile_input1

It's a good idea to make up your own example: work out a sequence of
operations for the register file to perform, figure out what input
signals are needed to achieve it, edit regfile_input1, and run the
simulation.

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
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, string " ", bits d, string " ",
>          bits sa, string " ", bits sb, string " ", bits x,
>          string "   Output: ", bits a, string " ", bits b]
>   in do putStrLn "\nSimulating register file (format output as binary)"
>         run input simoutput

The following is the same as sim_regfile, but it prints the output
using decimal representations.  The only difference appears in
simoutput, where bindec is used instead of bits for printing the binary
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
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, string " ", bindec 1 d, string " ",
>          bindec 1 sa, string " ", bindec 1 sb,
>          string " ", bindec 3 x,
>          string "   Output: ",
>          bindec 3 a, string " ", bindec 3 b]
>   in do putStrLn "\nSimulating register file (format output as decimal)"
>         run input simoutput



Simulate the Register File
~~~~~~~~~~~~~~~~~~~~~~~~~~


You don't need to read the simulation driver, which just handles the
formatting of the input and output signals.

> sim_regfile2 input =
>   let k =  4  -- there are 2^4 = 16 registers
>       n = 16  -- each register contains 16 bits
>       ld = getbit   input 0
>       d  = getbin k input 1
>       sa = getbin k input 2
>       sb = getbin k input 3
>       x  = getbin n input 4
>       (a,b) = regfile n k ld d sa sb x
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, string " ", bits d, string " ",
>          bits sa, string " ", bits sb, string " ", bits x,
>          string "\n       Output: ", bits a, string " ", bits b]
>   in do putStrLn "\nSimulating register file (format output as binary)"
>         run input simoutput

The following is the same as sim_regfile, but it prints the output
using decimal representations.  The only difference appears in
simoutput, where bindec is used instead of bits for printing the binary
numbers in decimal notation.

> sim_regfile_dec2 input =
>   let k =  4  -- there are 2^4 = 16 registers
>       n = 16  -- each register contains 16 bits
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
>   in do putStrLn "\nSimulating register file (format output as decimal)"
>         run input simoutput


Simulating the Register Transfer Machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

On each clock cycle, the register transfer machine can read out two
registers (specified by the sa, sb addresses), calculate a data value,
and load that data value into reg[d] if the load control ld=1.  The
data value can be either the data input x, or the sum produced by the
adder, and the add control signal determines which value is chosen.
If add=1 then the adder's output is selected and otherwise x is used.
Here is some sample input data, along with comments that describe what
is going on.

> rtm_input1 :: [[Int]]
> rtm_input1 =
> --      ld add d sa sb   x
> --      ~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 3, 0, 0, 125], -- R3 :=   x   = 125. R0=  0 R0=  0
>         [1, 0, 6, 3, 0,  10], -- R6 :=   x   =  10. R3=125 R0=  0
>         [1, 1, 2, 3, 6,   0], -- R2 := R3+R6 = 135. R3=125 R6= 10
>         [1, 0, 1, 1, 2,  75], -- R1 :=   x   =  75. R1=  0 R2=135
>         [1, 1, 1, 1, 2,   0], -- R1 := R1+R2 = 210. R1= 75 R2=135
>         [0, 0, 0, 1, 2,   0], -- nop                R1=210 R2=135
>         [0, 0, 0, 0, 0,   0]] -- nop                R0=  0 R0=  0

The simulation driver will print the input values for each clock
cycle, and then it will show the outputs produced by the register
transfer machine:
  -- reg[sa], the register addressed by sa
  -- reg[sb], the register addressed by sb
  -- the selected data value,
        which will either be reg[sa]+reg[sb] or x,
        and which *might* get loaded into a register
  -- the sum produced by the adder
       (this is the value of  reg[sa] + reg[sb]

You can run it by entering

  sim_rtm rtm_input1

Here's the simulation driver, which you can ignore.  It just takes
care of formatting and number conversions.

> sim_rtm input =
>   let n = 16  -- each register contains 16 bits
>       k =  4  -- there are 2^4 = 16 registers
>       ld  = getbit   input 0
>       add = getbit   input 1
>       d   = getbin k input 2
>       sa  = getbin k input 3
>       sb  = getbin k input 4
>       x   = getbin n input 5
>       (a,b,y,c,s) = rtm n k ld add d sa sb x
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, bit add, bindec 2 d, bindec 2 sa, bindec 2 sb,
>          bindec 4 x,
>          string "  Output: ",
>          bindec 4 a, bindec 4 b, bindec 4 y, bindec 4 s]
>   in do putStrLn "\nSimulate register transfer machine"
>         run input simoutput


Simulating the Register Transfer Machine
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

> sim_rtm2 =
>   let n = 30
>       k = 5
>       input =
> --      ld add d sa sb   x
> --      ~~~~~~~~~~~~~~~~~~~
>        [[1, 0, 3, 0, 0, 125], -- R3 :=   x   = 125. R0=  0 R0=  0
>         [1, 0, 6, 3, 0,  10], -- R6 :=   x   =  10. R3=125 R0=  0
>         [1, 1, 2, 3, 6,   0], -- R2 := R3+R6 = 135. R3=125 R6= 10
>         [1, 0, 1, 1, 2,  75], -- R1 :=   x   =  75. R1=  0 R2=135
>         [1, 1, 1, 1, 2,   0], -- R1 := R1+R2 = 210. R1= 75 R2=135
>         [0, 0, 0, 1, 2,   0], -- nop                R1=210 R2=135
>         [0, 0, 0, 0, 0,   0]]
>       ld  = getbit   input 0
>       add = getbit   input 1
>       d   = getbin k input 2
>       sa  = getbin k input 3
>       sb  = getbin k input 4
>       x   = getbin n input 5
>       (a,b,y,c,s) = rtm n k ld add d sa sb x
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Input: ",
>          bit ld, bit add, bindec 2 d, bindec 2 sa, bindec 2 sb,
>          bindec 4 x,
>          string "  Output: ",
>          bindec 4 a, bindec 4 b, bindec 4 y, bindec 4 s]
>   in do putStrLn "\nSimulate register transfer machine"
>         run input simoutput



Simulating the Multiplier
~~~~~~~~~~~~~~~~~~~~~~~~~

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
>     in do putStrLn "\nSimulate sequential multiplier"
>           run input spec


Running the simulations as batch job
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

This is a single operation that runs the various examples above as one
large batch.  You can run it by just launching Hydra, loading this
file, and entering "main" at the prompt.

> main =
>   do putStrLn "Hydra: running Tutorial1 batch"
>      sim_adder rippleAdd 16 add_input1
>      sim_sr4 sr4_testdata_1
>      sim_regfile
>      sim_regfile_dec
>      sim_rtm2
>      sim_mult
>      putStrLn "Examples1 finished"
