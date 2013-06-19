-- Example of how to use Template Haskell, from ghc Users's Guide

module Main where
import Printf (pr)

main = putStrLn ( $(pr "Hello") )


