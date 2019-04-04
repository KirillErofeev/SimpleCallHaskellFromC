GHC_INCLUDE = /usr/lib/ghc/include

test: clean all; ./../HaskellRL/codeball2018-linux/codeball2018 --p1 tcp-31003 --p2 helper --p1-dump ../out --no-countdown --log-file ../log --results-file ../r --duration 1801 & (sleep 1 && ./a.out)

all: Haskell.o Runner.o; ghc -no-hs-main *.o -lstdc++ -o a.out
Haskell.o: Haskell.hs; ghc -fforce-recomp *.hs
#main.o: main.cpp; g++ -std=c++11 -c main.cpp -I$(GHC_INCLUDE)
Runner.o: Runner.cpp; g++ -std=c++11 -c *.cpp csimplesocket/*.cpp -I$(GHC_INCLUDE)

.PHONY: clean
clean: ; rm -f *.o *.hi *_stub.h *.out
