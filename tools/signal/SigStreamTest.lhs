___________________________________________________________________
	 The Hydra Computer Hardware Description Language:
		   SigStream: Examples and Test

			  John O'Donnell
		       University of Glasgow
	       Copyright (c) 2000 by John O'Donnell

   See the readme file and http://www.dcs.gla.ac.uk/~jtod/Hydra/
___________________________________________________________________


> module TestSigStream where
> import Signal
> import SigBool
> import SigStream


> test1 = 
>   let x = forever zero :: Stream Bool
>       y = forever one
>       spec = [FmtBit fmtBit "x" x, FmtBit fmtBit "y" y]
>   in run 5 0 spec

> test2 =
>   let x = dff (inv x) :: Stream Bool
>   in run 10 0 [FmtBit fmtBit "x" x]

> test3 = 
>   let x :: [Stream Bool]
>       x = [forever zero, forever one, forever zero]
>       spec = [FmtWord fmtWord "x" 3 3 x]
>   in run 5 0 spec


