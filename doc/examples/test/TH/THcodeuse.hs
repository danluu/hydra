module THcodeuse where

import THcoderep

-- Define some things so the constructed code won't use unknown names...

abc = 123
data MyTree = Bcd | Def
fcn x = undefined

-- Splice in the generated code

$(exptestVar)
$(exptestCon)
$(exptestChar)
$(exptestInteger)
$(exptestApp)

$(dectest1)
$(dectest2)
$(dectest3)
$(dectest4)

$(showData)

