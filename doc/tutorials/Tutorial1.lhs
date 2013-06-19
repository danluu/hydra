----------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
	    Tutorial 1: Specifying and Simulating Circuits
			    John O'Donnell
----------------------------------------------------------------------

Introduction
~~~~~~~~~~~~

This document will give you a hands-on introduction to Hydra.  It
explains a series of experiments to try, and it also contains a
sprinkling of Hydra specifications to run.  Read this document at the
computer, trying each experiment in turn.  When you start designing
your own circuits, you can use the tutorial as a source of examples to
get you started.

This file is a "literate script": it's both an executable program and
a document for people to read.  The lines beginning with "> " (the
greater-than symbol, followed by space) are code that will be
executed, and all other lines are comment.

A Hydra file, like this one, begins with a statement that gives a name
to the module; this one is named Tutorial1.

> module Tutorial1 where

The file name should be Tutorial1.lhs, where Tutorial1 is the name of
the module.

After the "module" statement there should be one or more "import"
statements saying which other library modules need to be loaded.  The
first line, "import Hydra", loads all the Hydra software tools, and
the second one, "import CircuitLib", loads a collection of standard
basic circuits.  There are many other combinations of modules that can
be imported, depending on what you want to do; these will be explored
in subsequent tutorials.

> import Hydra        -- The basic system
> import CircuitLib   -- Library of basic circuits


Start the tutorial
~~~~~~~~~~~~~~~~~~

First, copy the Hydra/workspace directory to a directory in your own
file space, where you'll be able to edit and save files.  Set this as
your working directory, and start Hydra.  See the README file in the
Hydra/workspace directory for instructions on how to launch the
system.  You will actually be running an interactive Haskell
interpreter, with the Hydra libraries available.  Once the interpreter
starts, you should see a prompt, which might look like this:

  Type :? for help
  Prelude> 

Note that the :? command asks for Haskell help, not Hydra help!  Now
you're ready to start.  Begin by loading this file, Tutorial1.lhs, by
entering (after the "Prelude>" prompt) the command

  :load Tutorial1

Commands begin with :, and :load says to load a file.  (You can also
write it as :l, as in ":l Tutorial1").  The system will print a number
of messages as it loads the libraries, and then it will give another
prompt, which should look like this:

  Tutorial1> 

Now you can do simulations using all the definitions contained in this
file, as well as the standard Hydra definitions that were imported
above.


Simulate a logic gate
~~~~~~~~~~~~~~~~~~~~~

The simplest way to simulate a logic gate is to give it input signals
of type Bool, which has the values True and False.  Try the following
examples by entering each expression after the prompt and pressing
enter:

  inv True
  inv False
  and2 False True
  and2 True True

A convenient way to run such examples is to copy a line of the text in
this file, and then paste it into the window where you're executing
Hydra.  Copying the line "inv True" and pasting it into the Hydra
window should produce the following output:

*Tutorial1> inv True
False

The most commonly used logic gates are

  inv    the inverter
  and2   the 2-input and gate
  or2    the 2-input or gate
  xor2   the 2-input xor gate

There are also logic gates with three and four inputs; for example you
can evaluate

  and3 True False True
  xor4 False True True False

It is important to provide the correct number of inputs to any
component.  You cannot provide three inputs to a logic gate with only
two input ports, and you should always ensure that all inputs to a
logic gate are connected to a signal.

Running quick experiments by entering expressions like "inv True" can
be helpful, but it's also a good idea to include suitable test cases
as permanent definitions in a circuit specification module.  One way
to do this is to define a test case by a top-level equation, like
this:

> test_inv_1 = inv True   -- should be False
> test_inv_2 = inv False  -- should be True

Now you can run these test cases any time by entering the name
interactively:

  *Tutorial1> test_inv_1
  False

Although these examples are trivial, as you develop larger designs it
is helpful to have a collection of test cases, with the expected
results in comments.  These serve as examples of how to use the
circuits, and if a change to the file somewhere causes everything to
go wrong, you can easily run all the tests again to find out what is
working.


Using existing circuits
~~~~~~~~~~~~~~~~~~~~~~~

A number of useful standard circuits are defined in the modules that
were imported earlier.  For example, the multiplexor mux1 is defined
in the module circuits/bit/BitComb, and you can use it without
inserting its definition in your file.

> test_mux1_1 = mux1 True False True


Generating a truth table
~~~~~~~~~~~~~~~~~~~~~~~~

Usually when you test a circuit, it is necessary to simulate it with a
wide variety of inputs.  To make this easier, Hydra provides several
software tools.  The simplest of these tools is a set of functions
that generate the truth table for a combinational circuit by
simulating the circuit on all possible combinations of inputs.  For
example, entering the expression

  truthTable21 or2

will tell Hydra to simulate the or2 gate on all possible two-bit
inputs, and the resulting truth table will be printed:

*Tutorial1> truthTable21 or2
    0 0 | 0
    0 1 | 1
    1 0 | 1
    1 1 | 1

The "21" at the end of the name "truthTable21" indicates that a
circuit with two inputs and one output is expected.  A family of
similar functions can be used for circuits with various small numbers
of inputs and outputs.  Try, for example, entering the following
expressions:

  truthTable11 inv
  truthTable21 or2
  truthTable31 nand3
  truthTable41 xor4

The half adder circuit, which will be defined later, takes two inputs
and produces a pair of outputs.  The expression

  truthTable22 halfAdd

will cause Hydra to print the following result:

*Tutorial1> truthTable22 halfAdd
    0 0 | 0 0
    0 1 | 0 1
    1 0 | 0 1
    1 1 | 1 0

The truth table functions used so far  default labels for the columns,
which can sometimes make the tables confusing.  There are alternative
versions of the functions that let you specify strings for the column
headings.  These functions have an underscore at the ends of their
names, and they take the appropriate number of String arguments.

  truthTable21_ "input1" "input2" "output" xor2


Defining a new circuit
~~~~~~~~~~~~~~~~~~~~~~

A new circuit can be defined as a black box.  The definition states
the circuit's name, its type (what input and output ports must be
connected) and its internal structure (what components are present,
and how they are wired together).  Here is a simple example:

> circ1 :: Signal a => a -> a -> a
> circ1 x y = z
>   where z = and2 (inv x) y

The first line of the definition gives the circuit's name (circ1) and
its type (which appears after the ::).  The first part of the type,
Signal a =>, says that circ1 operates correctly on any signal
representation that is in the Signal class (this will be explained
later), and it takes two inputs with this signal type a, and returns
an output that has the same signal type a.  The second line of the
equation says that the black box for circ1 will use x and y as the
local names for the inputs, and z as the local name for the output.
Furthermore, the value of the z signal is produced by an and2 gate
with inputs (inv x) and y.  The behaviour of circ1 can be tested by
checking its truth table; when you enter

  truthTable21 circ1

the truth table that describes the function computed by circ1 will be
printed:

*Tutorial1> truthTable21 circ1
    0 0 | 0
    0 1 | 1
    1 0 | 0
    1 1 | 0


---------------------------------------------------------------------------
		   Synchronous Sequential Circuits
---------------------------------------------------------------------------

A sequential circuit has behavior that changes over time.  We will use
synchronous circuits, which require (1) every flip flop receives a
clock tick simultaneously, and (2) every feedback loop must go through
a flip flop.

Any circuit that contains a flip flop requires the synchronous model
to specify its behavior.  This, in turn, requires that the
representation of signals must be able to handle the system clock.  We
express that by the notation "Clocked a =>" in the circuit's type.

> seqcirc :: Clocked a => a -> a
> seqcirc x = y
>   where y = dff z
>         z = xor2 x y

Now the value of a Clocked signal is not simply True or False; it has
a value during every clock cycle.  A sequence of values over time is
called a stream.  The input data for seqcirc must therefore have a
type Stream Bool.  We can construct the signal by writing a list of
values, where the i'th element of the list is the input during clock
cycle i, and then converting the list to a stream with the listStream
function:

> test_input_1 :: Stream Bool
> test_input_1 =
>   listStream
>     [True,  False, True,  True,
>      False, True,  False, False,
>      True,  True,  True,  False]

The definition above says that the signal has value True during cycle
0, False during cycle 1, and so on.  Strictly speaking, the list
should be infinitely long because the clock runs -- at least in
principle -- forever.

Now we can simulate the circuit by applying it to its input signal.
but it's a good idea first to work out by hand the expected results.
Do this by making a table, where each column is a signal value and
each row is a clock cycle.  It's helpful to break the columns into
several categories: the clock cycle, the input signals, the flip flop
states, the outputs, and other internal signals.  For seqcirc, y is
both a flip flop state and an output of the circuit.  The table has
the form:

                  Output
  Cycle | Input | State | Signals
        |   x   |   y   |   z
  -------------------------------
    0   |   1       0
    1   |   0
    2   |   1
    3   |   1
   ...  |   0

Now initialize the table by setting the initial state of each flip
flop (i.e. its output during Cycle 0) to the flip flop power-up value,
and filling in some of the inputs.  For the Stream Bool model, the
initial flip flop state (and output value) is False (thus we assume
that the flip flop has a 0 when you first turn on power to the
circuit; in some of the other circuit models we will not make that
assumption).  To save space, We'll write 0 for False and 1 for True.
So the initial table is

                  Output
  Cycle | Input | State | Signals
        |   x   |   y   |   z
  -------------------------------
    0   |   1       0
    1   |   0
    2   |   1
    3   |   1
   ...  |   0

Now for each cycle, do the following:

(1) Simulate the settling down of the combinational logic by working
    out all the signal values.  Notice that when you work out the value
    of a combinational logic signal in a certain row of the table, you
    always use existing values from that same row.

    For Cycle 0, note that z = xor2 x y = xor2 1 0 = 1, giving:

                  Output
  Cycle | Input | State | Signals
        |   x   |   y   |   z
  -------------------------------
    0   |   1       0       1
    1   |   0
    2   |   1
    3   |   1
   ...  |   0

(2) Simulate the clock tick that ends the cycle: find the value of the
    input to a flip flop, and write it down as the flip flop's state
    for the *next* cycle.  Now the input to the flip flop y is the
    signal z, and z=1 in Cycle 0, so write down 1 as the flip flop
    state  y in the next row of the table:

                  Output
  Cycle | Input | State | Signals
        |   x   |   y   |   z
  -------------------------------
    0   |   1       0       1
    1   |   0       1
    2   |   1
    3   |   1
   ...  |   0

Continue the simulation, row by row.  You will alternately simulate a
clock cycle followed by a tick, then the next cycle, and so on, and as
time passes you'll fill the table from top to bottom.  After a few
cycles the table should look like this:

                  Output
  Cycle | Input | State | Signals
        |   x   |   y   |   z
  -------------------------------
    0   |   1       0       1
    1   |   0       1       1
    2   |   1       1       0
    3   |   1       0       1
    4   |   0       1       1
    5   |   1       1       0
    6   |   0       0       0
    7   |   0       0       0
    8   |   1       0       1
    9   |   1       1       0
   10   |   1       0       1
   11   |   0       1       1

Now, after simulating some test data by hand, enter the following
expression into Hydra:

  seqcirc test_input_1

Hydra performs the simulation and produces the following result:

*Tutorial1> seqcirc test_input_1
0,1,1,0,1,1,0,0,0,1,0,1,1,*** Exception: no more stream data from list

The output matches the results of the manual simulation.  The output
uses 1 and 0 rather than True and False; Hydra is still using True and
False to represent the signal values during a clock cycle, but it has
converted the Booleans to 0/1 automatically.  Since the input data
terminated after twelve cycles, an exception was thrown indicating
that there is no input data for cycle 12.  This message can be
ignored.
