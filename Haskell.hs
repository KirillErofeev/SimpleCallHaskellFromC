module Haskell where

foreign export ccall edwardHere :: IO ()

edwardHere = putStr "Edward Kmett is here too!"
