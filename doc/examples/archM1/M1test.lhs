---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
		    www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module M1test where

A collection of simulation drivers and test data for running the M1
system, a digital circuit implementation of a simple architecture.

To run the test program, load this module and then enter `main'

> import Signal
> import SigBool
> import SigStream
> import Group
> import Format
> import BitComb
> import BitWire
> import BitSeq
> import WordComb
> import WordWire
> import WordSeq
> import M1

The simulations will be carried out using the Stream Bool model:

> type Bit = Stream Bool
> type Word = [Bit]



---------------------------------------------------------------------------
		      Machine Language Programs
---------------------------------------------------------------------------

The program `findmax' searches an array of natural numbers for the
maximal element.  The end of the array is marked by -1, and the search
uses a while loop.

> findmax :: [String]
> findmax =

Register usage for findmax:
  R0 = constant 0
  R1 = maximum value found so far
  R2 = loop index i
  R3 = constant -1, indicates end of list
  R4 = x[i]
  R5 = constant 1, for incrementing i
  R6 = temp Bool value

> -- Machine Language         Assembly Language
> -- ~~~~~~~~~~~~~~~~         ~~~~~~~~~~~~~~~~~
>   ["1100", "0018",   -- 00       LOAD  R1,x[R0]     % max := x[0]
>    "2200", "0001",   -- 02       LDVAL R2,$0001     % i := 1
>    "2300", "ffff",   -- 04       LDVAL R3,$ffff     % R3 := -1
>    "2500", "0001",   -- 06       LDVAL R5,$0001     % R5 := 1 (for counter)
>    "1420", "0018",   -- 08 loop  LOAD  R4,x[R2]     % R4 := x[i]
>    "a643",           -- 0a       CMPGT R6,R4,R3     % R6 := (x[i] >= -1)
>    "c600", "0014",   -- 0b       JUMPF R6,done      % goto done if not
>    "a641",           -- 0d       CMPGT R6,R4,R1     % R6 := (x[i] > max)
>    "c600", "0011",   -- 0e       JUMPF R6,skip[R0]  % goto skip if not
>    "3140",           -- 10       ADD   R1,R4,R0     % max := x[i]
>    "3225",           -- 11 skip  ADD   R2,R2,R5     % i := i+1
>    "d000", "0008",   -- 12       JUMP  loop[R0]     % goto loop
>    "7100", "001e",   -- 14 done  STORE R1,max[R0]   % save max
>    "e000", "fffd",   -- 16       HALT               % terminate execution
>    "0002",           -- 18 x     DATA  $0002        %   2
>    "002a",           -- 19       DATA  $002a        %  42
>    "00e0",           -- 1a       DATA  $00e0        % 224
>    "0013",           -- 1b       DATA  $0013        %  19
>    "0004",           -- 1c       DATA  $0004        %   4
>    "ffff",           -- 1d       DATA  $ffff        %  -1
>    "0000"]           -- 1e max   DATA  $0000        %   0


---------------------------------------------------------------------------
			   Batch Test Cases
---------------------------------------------------------------------------

Run it by entering main at the prompt.

> main =
>   do let dashes_line = take 70 (repeat '-')
>      putStrLn "Running M1..."
>      putStrLn dashes_line
>      putStrLn "Running the M1 ALU..."
>      run_alu
>      putStrLn dashes_line
>      putStrLn "Running the M1 Datapath..."
>      run_datapath
>      putStrLn dashes_line
>      putStrLn "Executing findmax on the M1 architecture..."
>      run_prog findmax 150

---------------------------------------------------------------------------
		      Simulation driver for ALU
---------------------------------------------------------------------------

> run_alu = sim_alu 16 alu_input1

> sim_alu :: Int -> [[Int]] -> IO ()
> sim_alu n input =
>   let (cout,z) = alu n (a,b,c,d) x y
>       a = getbit   input 0
>       b = getbit   input 1
>       c = getbit   input 2
>       d = getbit   input 3
>       x = gettc n input 4
>       y = gettc n input 5
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Inputs:  ",
>          string " abcd = ", bit a, bit b, bit c, bit d,
>          string "\n         x = ", bits x, string " $", binhex x,
>          string " (bin:", bindec 5 x, string ")",
>          string " (tc: ", bitstc 6 x, string ")",
>          string "\n         y = ", bits y, string " $", binhex y,
>          string " (bin:", bindec 5 y, string ")",
>          string " (tc: ", bitstc 6 y, string ")",
>          string "\n       Outputs:  ",
>          string "cout=", bit cout,
>          string "\n         z = ", bits z, string " $", binhex z,
>          string " (bin:", bindec 5 z, string ")",
>          string " (tc: ", bitstc 6 z, string ")",
>          string "\n"]
>   in do putStrLn "\nALU simulation"
>         run input simoutput


---------------------------------------------------------------------------
		    Simulation driver for Datapath
---------------------------------------------------------------------------

> run_datapath = sim_datapath datapath_input1

> datapath_input1 :: [[Int]]
> datapath_input1 =
> -- rf  rf rf alu ir pc ad ma  x y   mem mem
> -- ld alu sd op  ld ld ld pc pc ad  sto dat
>  [[ 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,  0,  hexbin 16 "002a"], -- 42
>   [ 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,  0,  13], --R4=13
>   [ 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,  0,  hexbin 16 "abcd"],  --
>   [ 0, 0, 0,  0,  0, 0, 0, 0, 0, 0,  0,  20]
>  ]


Simulation Drivers
~~~~~~~~~~~~~~~~~~


> sim_datapath :: [[Int]] -> IO ()
> sim_datapath input =
>   let (ma,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath control memdat
>       control =
>         (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>          ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld,  ctl_ad_alu,
>          ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)

>       ctl_rf_ld  = getbit    input  0 :: Bit
>       ctl_rf_alu = getbit    input  1
>       ctl_rf_sd  = getbit    input  2
>       alu_opcode = getbin 4  input  3 :: Word
> --  ctl_alu_op = (alu_opcode!!0,alu_opcode!!1,alu_opcode!!2,alu_opcode!!3)
>       ctl_alu_op = (zero,zero,zero,zero)
>       ctl_ir_ld  = getbit    input  4 :: Bit
>       ctl_pc_ld  = getbit    input  5 :: Bit
>       ctl_ad_ld  = getbit    input  6 :: Bit
>       ctl_ad_alu = getbit    input  7 :: Bit
>       ctl_ma_pc  = getbit    input  7 :: Bit
>       ctl_x_pc   = getbit    input  8 :: Bit
>       ctl_y_ad   = getbit    input  9 :: Bit
>       ctl_sto    = getbit    input 10 :: Bit
>       memdat     = getbin 16 input 11 :: Word
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Datapath inputs:\n        ",
>          string " rf_ld=",  bit ctl_rf_ld,
>          string " rf_alu=", bit ctl_rf_alu,
>          string " rf_sd=",  bit ctl_rf_sd,
>          string " alu_op=", binhex alu_opcode,
>          string " ir_ld=",  bit ctl_ir_ld,
>          string " pc_ld=",  bit ctl_pc_ld,
>          string "\n        ",
>          string " ad_ld=",  bit ctl_ad_ld,
>          string " ma_pc=",  bit ctl_ma_pc,
>          string " x_pc= ",  bit ctl_x_pc,
>          string " y_ad=",   bit ctl_y_ad,
>          string " sto=",    bit ctl_sto,
>          string " memdat=", binhex memdat,
>
>          string "\n       Datapath outputs:\n        ",
>          string " ma=", binhex ma,
>          string " cond=",   bit cond,
>          string " a=",  binhex a, string " b=", binhex b,
>          string " ir=", binhex ir,
>          string " pc=", binhex pc,
>          string " ad=", binhex ad,
>          string "\n        ",
>          string " r=",  binhex r,
>          string " x=",  binhex x,
>          string " y=",  binhex y,
>          string " p=",  binhex p,
>          string "\n"
>         ]
>   in do putStrLn "\nDatapath simulation"
>         run input simoutput


---------------------------------------------------------------------------
		    Simulation driver for Control
---------------------------------------------------------------------------

> control_input1 :: [[Int]]
> control_input1 =
> --  reset   ir   cond
>   [[  1,     0,   0],
>    [  0,     0,   0],
>    [  0,     0,   0],
>    [  0,     0,   0],
>    [  0,     0,   0]
>   ]


> run_control = sim_control control_input1

> sim_control :: [[Int]] -> IO ()
> sim_control input =
>   let (state,ctl_signals) = control reset ir cond
>       reset = getbit    input 0 :: Bit
>       ir    = getbin 16 input 1 :: Word
>       cond  = getbit    input 2 :: Bit
>       (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>        ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld,  ctl_ad_alu,
>        ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
>         = ctl_signals
>       [st_instr_fet, st_nop,st_load0,st_load1,st_load2,st_ldval0,st_ldval1,
>        st_add,st_sub,st_neg,st_mul0,st_store0,st_store1,st_store2,
>        st_cmpeq,st_cmplt,st_cmpgt,st_jumpt0,st_jumpt1,st_jumpt2,
>        st_jumpf0,st_jumpf1,st_jumpf2,st_jump0,st_jump1,st_trape,st_trapf]
>         = state
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Control inputs\n        ",
>          string " reset=", bit reset,
>          string " ir=", bindec 4 ir,
>          string " cond=", bit cond,
>
>          string "\n       Control state\n        ",
>          string " st_instr_fet = ", bit st_instr_fet,
>          string "       st_nop = ",  bit st_nop,
>          string "     st_load0 = ", bit st_load0,
>          string "     st_load1 = ", bit st_load1,
>          string "\n        ",
>          string "     st_load2 = ", bit st_load2,
>          string "    st_ldval0 = ", bit st_ldval0,
>          string "    st_ldval1 = ", bit st_ldval1,
>          string "       st_add = ", bit st_add,
>          string "\n        ",
>          string "       st_sub = ", bit st_sub,
>          string "       st_neg = ", bit st_neg,
>          string "      st_mul0 = ", bit st_mul0,
>          string "    st_store0 = ", bit st_store0,
>          string "\n        ",
>          string "    st_store1 = ", bit st_store1,
>          string "    st_store2 = ", bit st_store2,
>          string "     st_cmpeq = ", bit st_cmpeq,
>          string "     st_cmplt = ", bit st_cmplt,
>          string "\n        ",
>          string "     st_cmpgt = ", bit st_cmpgt,
>          string "    st_jumpt0 = ", bit st_jumpt0,
>          string "    st_jumpt1 = ", bit st_jumpt1,
>          string "    st_jumpt2 = ", bit st_jumpt2,
>          string "\n        ",
>          string "    st_jumpf0 = ", bit st_jumpf0,
>          string "    st_jumpf1 = ", bit st_jumpf1,
>          string "    st_jumpf2 = ", bit st_jumpf2,
>          string "     st_jump0 = ", bit st_jump0,
>          string "\n        ",
>          string "     st_jump1 = ", bit st_jump1,
>          string "     st_trape = ", bit st_trape,
>          string "     st_trapf = ", bit st_trapf,
>
>          string "\n       Control signals\n        ",
>          string " ctl_rf_ld = ", bit ctl_rf_ld,
>          string " ctl_sto", bit ctl_sto,
>          string "\n"
>         ]
>   in do putStrLn "\nDatapath simulation"
>         run input simoutput


---------------------------------------------------------------------------
	     Simulation driver for  the full architecture
---------------------------------------------------------------------------

> run_m1 :: IO ()
> run_m1 = sim_m1 m1_input1

> m1_input1 :: [[Int]]
> m1_input1 =
> -- reset dma  dma_a  dma_d
> -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
>   [[ 0,   1,    0,  hexbin 16 "2100"], -- ldval R1,$a012
>    [ 0,   1,    1,  hexbin 16 "a012"],
>    [ 0,   1,    2,  hexbin 16 "2200"], -- ldval R2,$0ffe
>    [ 0,   1,    3,  hexbin 16 "0ffe"],
>    [ 0,   1,    4,  hexbin 16 "3312"], -- add   R3,R1,R2
>    [ 0,   1,    5,  hexbin 16 "3430"], -- add   R4,R3,R0
>    [ 1,   0,    0,  0],                -- Start
>    [ 0,   0,    0,  0],
>    [ 0,   0,    0,  0],
>    [ 0,   0,    0,  0],
>    [ 0,   0,    0,  0]
>   ]



> sim_m1 :: [[Int]] -> IO ()
> sim_m1 input =
>   let
>       reset = getbit    input 0 :: Bit
>       dma   = getbit    input 1 :: Bit
>       dma_a = getbin 16 input 2 :: Word
>       dma_d = getbin 16 input 3 :: Word
>
>       (ctl_state,ctl_signals,datapath_outputs,
>        m_sto,m_addr,m_real_addr,m_data,m_out)
>          = m1_system reset dma dma_a dma_d
>
>       (ma,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs
>
>       [st_instr_fet, st_dispatch, st_nop,st_load0,st_load1,st_load2,
>        st_ldval0,st_ldval1,
>        st_add,st_sub,st_neg,st_mul0,st_store0,st_store1,st_store2,
>        st_cmpeq,st_cmplt,st_cmpgt,st_jumpt0,st_jumpt1,st_jumpt2,
>        st_jumpf0,st_jumpf1,st_jumpf2,st_jump0,st_jump1,st_trape,st_trapf]
>         = ctl_state
>
>       (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>        ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld,  ctl_ad_alu,
>        ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
>         = ctl_signals
>
>       (ctl_alu_a,ctl_alu_b,ctl_alu_c,ctl_alu_d) = ctl_alu_op
>
>       simoutput :: [Format Bool]
>       simoutput =
>         [string "Computer system inputs\n        ",
>            string " reset=", bit reset, string " dma=", bit dma,
>            string " dma_a=", binhex dma_a, string " dma_d=", binhex dma_d,
>
>          string "\n\n       Control state\n        ",
>          string " st_instr_fet = ", bit st_instr_fet,
>          string "  st_dispatch = ", bit st_dispatch,
>          string "       st_nop = ",  bit st_nop,
>          string "     st_load0 = ", bit st_load0,
>          string "\n        ",
>          string "     st_load1 = ", bit st_load1,
>          string "     st_load2 = ", bit st_load2,
>          string "    st_ldval0 = ", bit st_ldval0,
>          string "    st_ldval1 = ", bit st_ldval1,
>          string "\n        ",
>          string "       st_add = ", bit st_add,
>          string "       st_sub = ", bit st_sub,
>          string "       st_neg = ", bit st_neg,
>          string "      st_mul0 = ", bit st_mul0,
>          string "\n        ",
>          string "    st_store0 = ", bit st_store0,
>          string "    st_store1 = ", bit st_store1,
>          string "    st_store2 = ", bit st_store2,
>          string "     st_cmpeq = ", bit st_cmpeq,
>          string "\n        ",
>          string "     st_cmplt = ", bit st_cmplt,
>          string "     st_cmpgt = ", bit st_cmpgt,
>          string "    st_jumpt0 = ", bit st_jumpt0,
>          string "    st_jumpt1 = ", bit st_jumpt1,
>          string "\n        ",
>          string "    st_jumpt2 = ", bit st_jumpt2,
>          string "    st_jumpf0 = ", bit st_jumpf0,
>          string "    st_jumpf1 = ", bit st_jumpf1,
>          string "    st_jumpf2 = ", bit st_jumpf2,
>          string "\n        ",
>          string "     st_jump0 = ", bit st_jump0,
>          string "     st_jump1 = ", bit st_jump1,
>          string "     st_trape = ", bit st_trape,
>          string "     st_trapf = ", bit st_trapf,
>
>          string "\n\n       Control signals\n        ",
>            string " ctl_alu_a  = ", bit ctl_alu_a,
>            string " ctl_alu_b  = ", bit ctl_alu_b,
>            string " ctl_alu_c  = ", bit ctl_alu_c,
>            string " ctl_alu_d  = ", bit ctl_alu_d,
>            string "\n        ",
>            string " ctl_rf_ld  = ",  bit ctl_rf_ld,
>            string " ctl_rf_alu = ", bit ctl_rf_alu,
>            string " ctl_rf_sd  = ",  bit ctl_rf_sd,
>            string " ctl_ir_ld  = ",  bit ctl_ir_ld,
>            string "\n        ",
>            string " ctl_pc_ld  = ",  bit ctl_pc_ld,
>            string " ctl_ad_ld  = ",  bit ctl_ad_ld,
>            string " ctl_ad_alu = ",  bit ctl_ad_alu,
>            string " ctl_ma_pc  = ",  bit ctl_ma_pc,
>            string "\n        ",
>            string " ctl_x_pc   = ",   bit ctl_x_pc,
>            string " ctl_y_ad   = ",   bit ctl_y_ad,
>            string " ctl_sto    = ",    bit ctl_sto,
>
>          string "\n\n       Datapath\n        ",
>            string "  ma = ", binhex ma,
>            string "   a = ", binhex a,
>            string "   b = ", binhex b,
>            string "  ir = ", binhex ir,
>            string "  pc = ", binhex pc,
>            string "  ad = ", binhex ad,
>            string "\n        ",
>            string "   r = ", binhex r,
>            string "   x = ", binhex x,
>            string "   y = ", binhex y,
>            string "   p = ", binhex p,
>            string " cnd = ", bit cond,
>
>          string "\n\n       Memory\n        ",
>            string " ctl_sto = ", bit ctl_sto,
>            string " m_sto = ", bit m_sto,
>            string " m_addr = ", binhex m_addr,
>            string " m_real_addr = ", binhex m_real_addr,
>            string "\n        ",
>            string " m_data = ", binhex m_data,
>            string " m_out =", binhex m_out,
>          string "\n"]
>   in do putStrLn "\nDatapath simulation"
>         run input simoutput

The following function takes a program (in the form of a list of
strings), loads it into the machine, and executes it.

> run_prog :: [String] -> Int -> IO ()
> run_prog prog n =
>   let inps = zipWith f [0..] prog ++ [[1,0,0,0]] ++ repeat [0,0,0,0]
>       f i x = [0, 1, i, hexbin 16 x]
>       m = n + length prog
>   in  sim_m1 (take m inps)


---------------------------------------------------------------------------
Miscellaneous
---------------------------------------------------------------------------

