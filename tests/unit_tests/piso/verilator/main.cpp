#include "Vpiso.h"
#include "verilated.h"
#include <iostream>
#include <bitset>
#include <cmath>

Vpiso * p;

constexpr int NUMBER_OF_STAGES = 6;

void toggleClock(){
    p->clk = 0;
    p->eval();
    p->clk = 1;
    p->eval();
}

void parallelIn(int val){
    p->enable = 1;
    p->uptake = 1;


    p->in0 = val&1;
    p->in1 = (val>>1) & 1;
    p->in2 = (val>>2) & 1;
    p->in3 = (val>>3) & 1;
    p->in4 = (val>>4) & 1;
    p->in5 = (val>>5) & 1;

    toggleClock();
}

int shiftOut(){
    int sum = 0;
    p->uptake = 0;
    p->enable =1;
    p->eval();
    for(int i = 0; i < NUMBER_OF_STAGES; i++){
        sum = (sum << 1) + p->out;
        toggleClock();
    }
    return sum;
}

int main(int argc, char ** argv, char **env){
    Verilated::commandArgs(argc, argv);
    p = new Vpiso;

    toggleClock();
    int number = atoi(argv[argc-1]);

    // Providing a seed value
	srand((unsigned) time(NULL));

	// Get a random number
    for(int i =0; i<number;i++){
        int x = rand() % 0b111111;
        parallelIn(x);
        int a = shiftOut();
        assert(a==x);
    }
    std::cerr<<number<<", ";
}