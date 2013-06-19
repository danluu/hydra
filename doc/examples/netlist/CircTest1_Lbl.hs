{- This module splices in the definitions found in CircTest1, after
they have been transformed.  This is a helper module, which could be
generated automatically.  The reason it's needed is that Template
Haskell requires the spliced definitions to appear in a separate
file. (?)

Note: the prototype definition of transform_module just transforms the
first definition in the module; it ignores the rest.  To be fixed. -}


module CircTest1_Lbl where

import Language.Haskell.THSyntax
import Signal
import Transform
import CircTest1_Code

$(transform_module circ_defs_rep)

-- circ_defs_rep is defined in CircTest1.
