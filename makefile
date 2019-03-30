GHC_INCLUDE = /usr/lib/ghc/include

all: Haskell.o main.o; ghc -no-hs-main *.o -lstdc++ -o a.out
Haskell.o: Haskell.hs; ghc -fforce-recomp Haskell.hs
main.o: main.cpp; g++ -std=c++11 -c main.cpp -I$(GHC_INCLUDE)

.PHONY: clean		
clean: ; rm -f *.o *.hi *_stub.h *.out
