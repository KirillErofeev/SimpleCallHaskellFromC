module Haskell where

import Types

foreign export ccall edwardHere :: IO ()

edwardHere = putStr $ "Edward Kmett is here too!" ++ t 
