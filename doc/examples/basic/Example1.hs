module Example1 where

import Signal
import SigBool
import CombTools

testcirc :: Signal a => a -> a -> a -> a
testcirc a b c = or2 (and2 a b) c

circuit1 a b c = inv (xor2 (and2 a b) (or2 b c))
