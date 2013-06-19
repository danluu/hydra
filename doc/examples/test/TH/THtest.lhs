Basic experiments with Template Haskell.

These are are working and should stay that way...


In main... 

> -- $(arith_fun_defs_rep)
> -- $(foo)

In imported file...

> arith_fun_defs_rep =
>   [d|
>       f1 :: Int -> Int -> (Int,Int)
>       f1 a b = (a+1, f2 b)
>
>       f2 :: Int -> Int
>       f2 x = 3*x
>    |]

> foo :: Q [Dec]
> foo =
>   do x <- circ_defs_rep
> -- The definitions with undefined are good for checking that I'm
> -- using the types correctly, but they can't be executed at compile time
> --     let y = Val undefined undefined undefined
> --     z <- val undefined undefined undefined
> --     a <- fun "funa" undefined
> --     b <- fun "funb" [clause undefined undefined undefined,
> --                      clause undefined undefined undefined]
>      c <- fun "func" [clause
>                          [pvar "x", pvar "y"]
>                          (normal (var "y"))
>                          []]
>      return [c]
