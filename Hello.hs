module Hello where

foreign export ccall helloFromHaskell :: Int

helloFromHaskell :: Int
helloFromHaskell = 22222
