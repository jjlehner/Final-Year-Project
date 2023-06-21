// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Symbol table internal header
//
// Internal details; most calling programs do not need this header,
// unless using verilator public meta comments.

#ifndef VERILATED_VPISO__SYMS_H_
#define VERILATED_VPISO__SYMS_H_  // guard

#include "verilated.h"

// INCLUDE MODEL CLASS

#include "Vpiso.h"

// INCLUDE MODULE CLASSES
#include "Vpiso___024root.h"

// SYMS CLASS (contains all model state)
class Vpiso__Syms final : public VerilatedSyms {
  public:
    // INTERNAL STATE
    Vpiso* const __Vm_modelp;
    bool __Vm_didInit = false;

    // MODULE INSTANCE STATE
    Vpiso___024root                TOP;

    // CONSTRUCTORS
    Vpiso__Syms(VerilatedContext* contextp, const char* namep, Vpiso* modelp);
    ~Vpiso__Syms();

    // METHODS
    const char* name() { return TOP.name(); }
} VL_ATTR_ALIGNED(VL_CACHE_LINE_BYTES);

#endif  // guard
