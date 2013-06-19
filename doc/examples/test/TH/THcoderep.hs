module THcoderep where

import Language.Haskell.THSyntax

makecode =
  do
     qIO (putStrLn "character literal")
     let code = Char 'x'
     qIO (putStrLn (show code))

     qIO (putStrLn "\nClause normal, with no decls\n")
     let rhs = Normal (Var "v1")
     let code = Clause [Pvar "p1", Pvar "p2"]
                  rhs []
     qIO (putStrLn (show code))

     qIO (putStr "\n")

     let dummydec = undefined -- Val (Pvar "dummypat") rhs []
     returnQ dummydec

-- Expressions

mkExpDef :: String -> Exp -> Q [Dec]
mkExpDef name exp =
  do qIO (putStrLn ("\nDefining " ++ name ++ " = " ++ show exp))
     let def = Val (Pvar name) (Normal exp) []
     returnQ [def]

-- Variables and constructors

exptestVar = mkExpDef "etVar" (Var "abc")
exptestCon = mkExpDef "etCon" (Con "Bcd")

-- Literal expressions

exptestChar = mkExpDef "etChar" (Lit (Char 'a'))
exptestInteger = mkExpDef "etInteger" (Lit (Integer 123))
exptestApp = mkExpDef "etApp" (App (Var "fcn") (Var "abc"))

-- Declarations

mkDec :: Dec -> Q [Dec]
mkDec d =
  do qIO (putStrLn ("\n Declaration: " ++ show d))
     returnQ [d]

-- fcn1 a b = 987
dectest1 = mkDec $ Fun "fcn1"
                     [Clause [Pvar "a", Pvar "b"]
                       (Normal (Lit (Integer 987)))
                       []
                     ]

dectest2 = mkDec $ Fun "fcn2"
                     [Clause [Pvar "a", Pvar "b"]
                       (Normal (Lit (Integer 987)))
                       [],
                      Clause [Pvar "c", Pvar "d"]
                       (Normal (Lit (Integer 876)))
                       []
                     ]


dectest3 = mkDec $ Fun "fcn3"
                     [Clause [Pvar "a", Pvar "b"]
                       (Normal (Lit (Integer 987)))
                       [Val (Pvar "p1") (Normal (Lit (Integer 51))) [],
                        Val (Pvar "p2") (Normal (Lit (Integer 52))) []
                       ]
                     ]


dectest4 = mkDec $ Fun "fcn4"
                     [Clause [Pvar "a", Pvar "b"]
                       (Normal (Lit (Integer 987)))
                       [Val (Pvar "p1") (Normal (Lit (Integer 51))) [],
                        Val (Pvar "p2") (Normal (Lit (Integer 52))) []
                       ],
                      Clause [Pvar "c", Pvar "d"]
                       (Normal (Lit (Integer 876)))
                       [Val (Pvar "p3") (Normal (Lit (Integer 61))) [],
                        Val (Pvar "p4") (Normal (Lit (Integer 62))) []
                       ]
                     ]


g1 [] = a+b
  where a = 4
        b = 5
g1 (x:xs) = b-a
  where a = 25
        b = 11



{- not ok, a and b aren't in scope on first equation...
g2 [] = a+b
g2 (x:xs) = b-a
  where a = 25
        b = 11
-}


showData =
  do code <- [d| data Foo = Bar | Baz |]
     qIO (putStrLn "original code is: data Foo = Bar | Baz")
     qIO (putStrLn (show code))
     return code




