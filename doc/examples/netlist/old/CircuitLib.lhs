---------------------------------------------------------------------------
      Hydra: A Functional Computer Hardware Description Language
	 --- StdCircuit.lhs: The Standard Circuit Library ---
	  See the README and COPYING files and the web page:
		    www.dcs.gla.ac.uk/~jtod/Hydra/
---------------------------------------------------------------------------

This module provides the full Hydra library of standard circuits.

> module CircuitLib
>  (module BitWire,  module BitComb,  module BitSeq,
>   module WordWire, module WordComb, module WordSeq,
>   module Word4Comb, module Word4Seq
>  )
> where

> import BitWire
> import BitComb
> import BitSeq
> import Word4Comb
> import Word4Seq
> import WordWire
> import WordComb
> import WordSeq

The actual circuits are defined in several more specialised modules,
which are listed above.  The purpose of StdCircuit is to collect the
standard circuits together in one module.  This enables the user to
get the full Hydra library by importing just two modules: Hydra and
StdCircuit.
