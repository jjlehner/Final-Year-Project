// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vpiso.h for the primary calling header

#include "verilated.h"

#include "Vpiso__Syms.h"
#include "Vpiso___024root.h"

void Vpiso___024root___ctor_var_reset(Vpiso___024root* vlSelf);

Vpiso___024root::Vpiso___024root(Vpiso__Syms* symsp, const char* name)
    : VerilatedModule{name}
    , vlSymsp{symsp}
 {
    // Reset structure values
    Vpiso___024root___ctor_var_reset(this);
}

void Vpiso___024root::__Vconfigure(bool first) {
    if (false && first) {}  // Prevent unused
}

Vpiso___024root::~Vpiso___024root() {
}
