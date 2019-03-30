#include <iostream>
#include "Haskell_stub.h"

int main(int argc, char** argv){
    hs_init(&argc, &argv);
    std::cout << "BJORN IS HERE" << std::endl;
    edwardHere();
    hs_exit();

}
