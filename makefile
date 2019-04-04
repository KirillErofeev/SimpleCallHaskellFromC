ifeq ($(CXX),g++)
    GHC_INCLUDE = /usr/lib/ghc/include
endif
ifeq ($(CXX),clang)
    GHC_INCLUDE = /usr/local/Cellar/ghc/8.6.4/lib/ghc-8.6.4/include
endif

test: clean all; ./codeball2018/codeball2018 --p1 tcp-31003 --p2 helper --p1-dump ../out --no-countdown --log-file ../log --results-file ../r --duration 1801 & (sleep 1 && ./a.out)

testAgainstEmpty: clean all; ./codeball2018/codeball2018 --p1 tcp-31003 --p2 empty --p1-dump ../out --no-countdown --log-file ../log --results-file ../r --duration 1801 & (sleep 1 && ./a.out)

testAgainstEmptyWithNoShow: clean all; ./codeball2018/codeball2018 --p1 tcp-31003 --p2 empty --p1-dump ../out --no-countdown --log-file ../log --results-file ../r --noshow --duration 1801 & (sleep 1 && ./a.out)

all: Haskell.o Runner.o; ghc -no-hs-main *.o -lstdc++ -o a.out
Haskell.o: Haskell.hs; ghc -fforce-recomp *.hs
#main.o: main.cpp; g++ -std=c++11 -c main.cpp -I$(GHC_INCLUDE)
Runner.o: Runner.cpp; g++ -std=c++11 -c *.cpp csimplesocket/*.cpp -I$(GHC_INCLUDE)

.PHONY: clean
clean: ; rm -f *.o *.hi *_stub.h *.out
