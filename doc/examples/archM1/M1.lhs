---------------------------------------------------------------------------
	   The Hydra Computer Hardware Description Language
 See the README and COPYING files, and www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

> module M1 where

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


---------------------------------------------------------------------------
			Arithmetic/Logic Unit
---------------------------------------------------------------------------


  |~~~~~~~~~~~~~~~~~~~~~~~~|
  | Function |  a,b   c,d  |
  |~~~~~~~~~~|~~~~~~~~~~~~~|
  |   x+y    |  0 0        |
  |   x-y    |  0 1        |
  |    -x    |  1 0        |
  |   x+1    |  1 1   0 0  |
  |   x<y    |  1 1   0 1  |
  |   x=y    |  1 1   1 0  |
  |   x>y    |  1 1   1 1  |
  ~~~~~~~~~~~~~~~~~~~~~~~~~~

alu_add = 0000
alu_sub = 0100
alu_neg = 1000
alu_inc = 1100
alu_lt  = 1101
alu_eq  = 1110
alu_gt  = 1111

> alu_input1 :: [[Int]]
> alu_input1 =
> --  a  b  c  d     x      y        Operation  Result
> -- ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
>   [[0, 0, 0, 0,    14,    15],   --   x+y       29
>    [0, 0, 0, 0,   125,   590],   --   x+y      715
>    [0, 0, 0, 0,    49,    15],   --   x+y       64
>    [0, 0, 0, 0,    21,   -19],   --   x+y        2
>    [0, 0, 0, 0,    21,   -35],   --   x+y      -14
>    [0, 0, 0, 0,  -350,    75],   --   x+y     -275
>    [0, 0, 0, 0,  -420,   -90],   --   x+y     -510
>
>    [0, 1, 0, 0,    49,    15],   --   x-y       34
>    [0, 1, 0, 0,    15,    49],   --   x-y      -34
>
>    [1, 0, 0, 0,    39,     0],   --   -x       -39
>    [1, 0, 0, 0,    25,    70],   --   -x       -25
>
>    [1, 1, 0, 0,    17,     0],   --   x+1       18
>    [1, 1, 0, 0,   193,    52],   --   x+1      194
>
>    [1, 1, 0, 1,   150,   175],   --   x<y        1
>    [1, 1, 1, 0,   150,   175],   --   x=y        0
>    [1, 1, 1, 1,   150,   175],   --   x>y        0
>
>    [1, 1, 0, 1,     9,     9],   --   x<y        0
>    [1, 1, 1, 0,     9,     9],   --   x=y        1
>    [1, 1, 1, 1,     9,     9],   --   x<y        0
>
>    [1, 1, 0, 1,    23,    17],   --   x<y        0
>    [1, 1, 1, 0,    23,    17],   --   x=y        0
>    [1, 1, 1, 1,    23,    17]    --   x>y        1
>   ]


> alu n (a,b,c,d) x y =
>   let wzero = fanoutbuf n zero
>       wone = boolword n one
>       negating = xor2 a b
>       comparing = and3 a b (or2 c d)
>       x' = mux2w (a,b) x x wzero x
>       y' = mux2w (a,b) y (winv y) (winv x) (mux1w (or2 c d) wone y)
>       xy = bitslice2 x' y'
>       (cout,sum) = rippleAdd negating xy
>       (lt,eq,gt) = rippleCmp xy
>       lt_tc = mux2 (xy!!0) lt zero one lt
>       eq_tc = eq
>       gt_tc = mux2 (xy!!0) gt one zero gt
>       comp_bool = mux2 (c,d) zero lt_tc eq_tc gt_tc
>       comp_word = boolword n comp_bool
>       z = mux1w comparing sum comp_word
>   in (cout,z)


---------------------------------------------------------------------------

Control signals
~~~~~~~~~~~~~~~

  ctl_rf_ld    Load  register file (if 0, remain unchanged)
  ctl_rf_alu   Input to register file is ALU output (if 0, use m)
  ctl_rf_sd    Use ir_d as source a address (if 0, use ir_sa)
  ctl_alu_a    4-bit alu operation code (see section on the ALU)
  ctl_alu_b      "
  ctl_alu_c      "
  ctl_alu_d      "
  ctl_ir_ld    Load ir register (if 0, remain unchanged)
  ctl_pc_ld    Load pc register (if 0, remain unchanged)
  ctl_ad_ld    Load ad register (if 0, remain unchanged)
  ctl_ad_alu   Obtain ad input from alu (if 0, from memory data input)
  ctl_ma_pc    Transmit pc on memory address bus (if 0, transmit addr)
  ctl_x_pc     Transmit pc on x (if 0, transmit reg[sa])
  ctl_y_ad     Transmit ad on y (if 0, transmit reg[sb])
  ctl_sto      Memory store (if 0, fetch)


---------------------------------------------------------------------------
			       Datapath
---------------------------------------------------------------------------


> datapath control memdat = (ma,cond,a,b,ir,pc,ad,ovfl,r,x,y,p)
>   where

Give names to the individual control signals

>       (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>        ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld,  ctl_ad_alu,
>        ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
>          = control

Specify the size parameters

>       n = 16
>       k =  4

Registers

>       (a,b) = regfile n k ctl_rf_ld ir_d rf_sa rf_sb p
>       ir = reg n ctl_ir_ld memdat
>       pc = reg n ctl_pc_ld r
>       ad = reg n ctl_ad_ld (mux1w ctl_ad_alu memdat r)

ALU

>       (ovfl,r) = alu n ctl_alu_op x y

Internal processor signals

>       x  = mux1w ctl_x_pc a pc    -- alu input 1
>       y  = mux1w ctl_y_ad b ad    -- alu input 2
>       rf_sa = mux1w ctl_rf_sd ir_sa ir_d
>       rf_sb = ir_sb
>       p  = mux1w ctl_rf_alu memdat r  -- data input to register file
>       ma = mux1w ctl_ma_pc ad pc  -- memory address
>       cond = any1 a
>       ir_op = field ir  0 4       -- instruction opcode
>       ir_d  = field ir  4 4       -- instruction destination register
>       ir_sa = field ir  8 4       -- instruction source a register
>       ir_sb = field ir 12 4       -- instruction source b register


---------------------------------------------------------------------------
			  Control Algorithm
---------------------------------------------------------------------------

repeat forever
  st_instr_fet:
    ir := mem[pc], pc++;
       {ctl_ma_pc, ctl_ir_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
  st_dispatch:
  case ir_op of
    0 nop:
    1 load:
        st_load0:
          ad := mem[pc], pc++;
             {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abcd=1100, ctl_pc_ld}
        st_load1:
          ad := reg[ir_sa] + ad
             {set ctl_y_ad, ctl_alu_abcd=0000, set ctl_ad_ld}
        st_load2:
          reg[ir_d] := mem[ad]
             {ctl_rf_ld}
    2 ldval:
        st_ldval0:
          ad := mem[pc], pc++;
             {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abcd=1100, ctl_pc_ld}
        st_ldval1:
          reg[ir_d] := reg[ir_sa] + ad
             {ctl_y_ad, ctl_alu=alu_add, ctl_rf_alu,ctl_rf_ld}
    3 add:
        st_add:
          reg[ir_d] := reg[ir_sa] + reg[ir_sb]
             {ctl_alu_abcd=0000, ctl_rf_alu, ctl_rf_ld}
    4 sub:
        st_sub:
          reg[ir_d] := reg[ir_sa] - reg[ir_sb]
             {ctl_alu_abcd=0100, ctl_rf_alu, ctl_rf_ld}
    5 neg:
        st_neg:
          reg[ir_d] := - reg[ir_sa]
             {ctl_alu_abcd=1000, ctl_rf_alu, ctl_rf_ld}
    6 mul:
        st_mul0:
    7 store:
        st_store0:
          ad := mem[pc], pc++;
             {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
        st_store1:
          ad := reg[ir_sa] + ad
             {set aluop=(+), set ctl_b_addr, set ctl_ad_ld}
        st_store2:
          mem[addr] := reg[ir_d]
             {ctl_rf_sd, ctl_sto}
    8 cmpeq:
        st_cmpeq:
          reg[ir_d] := reg[ir_sa] = reg[ir_sb]
             {ctl_alu_abcd=1110, ctl_rf_alu, ctl_rf_ld}
    9 cmplt:
        st_cmplt:
          reg[ir_d] := reg[ir_sa] < reg[ir_sb]
             {ctl_alu_abcd=1101, ctl_rf_alu, ctl_rf_ld}
    a cmpgt:
        st_cmpgt:
          reg[ir_d] := reg[ir_sa] > reg[ir_sb]
             {ctl_alu_abcd=1111, ctl_rf_alu, ctl_rf_ld}
    b jumpt:
        case all0 (reg[ir_sa]) of
          False:
            st_jumpt0:
              ad := mem[pc], pc++;
                 {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
            st_jumpt1:
              pc := reg[ir_sa] + ad
                 {ctl_y_ad, ctl_alu=alu_add, ctl_pc_ld}
          True:
            st_jumpt2:
    c jumpf:
        st_jumpf0:
          ad := mem[pc], pc++;
            {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu_abcd=1100, ctl_pc_ld,
             ctl_rf_sd}
          case reg[ir_sa] = 0 of
            False:
              break
            True:
              st_jumpf1:
                pc := reg[ir_sa] + ad
                   {ctl_y_ad, ctl_alu_abcd=0000, ctl_pc_ld}
    d jump:
        st_jump0:
          ad := mem[pc], pc++;
             {ctl_ma_pc, ctl_ad_ld, ctl_x_pc, ctl_alu=alu_inc, ctl_pc_ld}
        st_jump1:
          pc := reg[ir_sa] + ad
             {ctl_y_ad, ctl_alu=alu_add, ctl_pc_ld}
    e trap:
       st_trape:
    f trap:
       st_trapf:

Control circuit
~~~~~~~~~~~~~~~

> control :: Clocked a => a -> [a] -> a
>   -> ([a], (a,a,a,(a,a,a,a),a,a,a,a,a,a,a,a))

> control reset ir cond = (state,ctl_signals)
>   where

>       ir_op = field ir  0 4       -- instruction opcode
>       ir_d  = field ir  4 4       -- instruction destination register
>       ir_sa = field ir  8 4       -- instruction source a register
>       ir_sb = field ir 12 4       -- instruction source b register

>       start = foldl1 or2
>         [reset,st_nop,st_load2,st_ldval1,st_add,st_sub,st_neg,
>          st_mul0,st_store2,st_cmpeq,st_cmplt,st_cmpgt,
>          st_jumpt2,
>          st_jumpf1, and2 st_jumpf0 cond,
>          st_jump1,st_trape,st_trapf]

>       st_instr_fet = dff start
>       st_dispatch  = dff st_instr_fet
>       p = demux4w ir_op st_dispatch

>       st_nop    = dff (p!!0)
>       st_load0  = dff (p!!1)
>       st_load1  = dff st_load0
>       st_load2  = dff st_load1
>       st_ldval0 = dff (p!!2)
>       st_ldval1 = dff st_ldval0
>       st_add    = dff (p!!3)
>       st_sub    = dff (p!!4)
>       st_neg    = dff (p!!5)
>       st_mul0   = dff (p!!6)
>       st_store0 = dff (p!!7)
>       st_store1 = dff st_store0
>       st_store2 = dff st_store1
>       st_cmpeq  = dff (p!!8)
>       st_cmplt  = dff (p!!9)
>       st_cmpgt  = dff (p!!10)
>       st_jumpt0 = dff (p!!11)
>       st_jumpt1 = dff st_jumpt0
>       st_jumpt2 = dff st_jumpt1
>       st_jumpf0 = dff (p!!12)
>       st_jumpf1 = dff (and2 st_jumpf0 (inv cond))
>       st_jumpf2 = dff zero -- st_jumpf1         REMOVE ???
>       st_jump0  = dff (p!!13)
>       st_jump1  = dff st_jump0
>       st_trape  = dff (p!!14)
>       st_trapf  = dff (p!!15)

>       state =
>         [st_instr_fet, st_dispatch, st_nop, st_load0, st_load1, st_load2,
>          st_ldval0, st_ldval1,
>          st_add, st_sub, st_neg, st_mul0, st_store0, st_store1, st_store2,
>          st_cmpeq, st_cmplt, st_cmpgt, st_jumpt0, st_jumpt1, st_jumpt2,
>          st_jumpf0, st_jumpf1, st_jumpf2, st_jump0, st_jump1, st_trape,
>          st_trapf]

>       ctl_rf_ld   = orw [st_load2,st_ldval1,st_add,st_sub,
>                            st_neg,st_cmpeq,st_cmplt,st_cmpgt]
>       ctl_rf_alu  = orw [st_ldval1,st_add,st_sub,st_neg,st_cmpeq,
>                            st_cmplt,st_cmpgt]
>       ctl_rf_sd   = orw [st_store2,st_jumpf0]
>       ctl_alu_a   = orw [st_instr_fet,st_load0,st_ldval0,st_neg,st_cmpeq,
>                            st_cmplt,st_cmpgt,st_jumpf0]
>       ctl_alu_b   = orw [st_instr_fet,st_load0,st_ldval0,st_sub,st_cmpeq,
>                            st_cmplt,st_cmpgt,st_jumpf0]
>       ctl_alu_c   = orw [st_cmpeq,st_cmpgt]
>       ctl_alu_d   = orw [st_cmpeq,st_cmplt,st_cmpgt]
>       ctl_ir_ld   = orw [st_instr_fet]
>       ctl_pc_ld   = orw [st_instr_fet,st_load0,st_ldval0,st_store0,
>                            st_jumpt0,st_jumpt1,st_jumpf0,st_jumpf1,
>                            st_jump0,st_jump1]
>       ctl_ad_ld   = orw [st_load0,st_load1,st_ldval0,st_store0,
>                            st_store1,st_jumpt0,st_jumpf0,st_jump0]
>       ctl_ad_alu  = orw [st_load1]
>       ctl_ma_pc   = orw [st_instr_fet,st_load0,st_ldval0,st_store0,
>                            st_jumpt0,st_jumpf0,st_jump0]
>       ctl_x_pc    = orw [st_instr_fet,st_load0,st_ldval0,st_store0,
>                            st_jumpt0,st_jumpf0,st_jump0]
>       ctl_y_ad    = orw [st_load1,st_ldval1,st_jumpt1,st_jumpf1,st_jump1]
>       ctl_sto     = orw [st_store2]
>       ctl_alu_op = (ctl_alu_a,ctl_alu_b,ctl_alu_c,ctl_alu_d)
>
>       ctl_signals =
>         (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>          ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld,  ctl_ad_alu,
>          ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)

This circuit extracts the ctl_sto field from the entire control word.

> get_ctl_sto (ctl_rf_ld,  ctl_rf_alu, ctl_rf_sd,  ctl_alu_op,
>    ctl_ir_ld,  ctl_pc_ld,  ctl_ad_ld, ctl_ad_alu,
>    ctl_ma_pc,  ctl_x_pc,   ctl_y_ad,   ctl_sto)
>      = ctl_sto

> m1_system reset dma dma_a dma_d =
>   let n = 16
>       (ctl_state,ctl_signals) = control reset ir cond
>       ctl_sto = get_ctl_sto ctl_signals
>       datapath_outputs = datapath ctl_signals m_out
>       (ma,cond,a,b,ir,pc,ad,ovfl,r,x,y,p) = datapath_outputs
>       m_sto = or2 dma ctl_sto
>       m_addr = mux1w dma ma dma_a
>       m_real_addr = field m_addr (n-8) 8
>       m_data = mux1w dma p dma_d
>       m_out = memw n 8 m_sto m_real_addr m_data
>   in (ctl_state,ctl_signals,
>       datapath_outputs,
>       m_sto, m_addr, m_real_addr, m_data, m_out)
