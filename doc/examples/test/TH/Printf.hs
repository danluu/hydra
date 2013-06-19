module Printf where

import Language.Haskell.THSyntax

data Format = D | S | L String

parse :: String -> [Format]
parse s = [L s]

gen :: [Format] -> ExpQ
gen [D]   = [| \n -> show n |]
gen [S]   = [| \s -> s |]
gen [L s] = string s

pr :: String -> ExpQ
pr s = gen (parse s)
