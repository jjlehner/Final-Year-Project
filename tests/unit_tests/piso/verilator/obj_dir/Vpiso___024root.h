// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design internal header
// See Vpiso.h for the primary calling header

#ifndef VERILATED_VPISO___024ROOT_H_
#define VERILATED_VPISO___024ROOT_H_  // guard

#include "verilated.h"

class Vpiso__Syms;

class Vpiso___024root final : public VerilatedModule {
  public:

    // DESIGN SPECIFIC STATE
    VL_IN8(clk,0,0);
    VL_IN8(uptake,0,0);
    VL_IN8(enable,0,0);
    VL_IN8(in0,0,0);
    VL_IN8(in1,0,0);
    VL_IN8(in2,0,0);
    VL_IN8(in3,0,0);
    VL_IN8(in4,0,0);
    VL_IN8(in5,0,0);
    VL_OUT8(out,0,0);
    CData/*0:0*/ piso__DOT__a1;
    CData/*0:0*/ piso__DOT__a2;
    CData/*0:0*/ piso__DOT__a3;
    CData/*0:0*/ piso__DOT__a4;
    CData/*0:0*/ piso__DOT__a5;
    CData/*0:0*/ piso__DOT__a6;
    CData/*0:0*/ piso__DOT__s0__DOT__nextprime;
    CData/*0:0*/ piso__DOT__s1__DOT__nextprime;
    CData/*0:0*/ piso__DOT__s2__DOT__nextprime;
    CData/*0:0*/ piso__DOT__s3__DOT__nextprime;
    CData/*0:0*/ piso__DOT__s4__DOT__nextprime;
    CData/*0:0*/ piso__DOT__s5__DOT__nextprime;
    CData/*0:0*/ __Vtrigrprev__TOP__clk;
    CData/*0:0*/ __VactContinue;
    IData/*31:0*/ __VstlIterCount;
    IData/*31:0*/ __VicoIterCount;
    IData/*31:0*/ __VactIterCount;
    VlTriggerVec<1> __VstlTriggered;
    VlTriggerVec<1> __VicoTriggered;
    VlTriggerVec<1> __VactTriggered;
    VlTriggerVec<1> __VnbaTriggered;

    // INTERNAL VARIABLES
    Vpiso__Syms* const vlSymsp;

    // CONSTRUCTORS
    Vpiso___024root(Vpiso__Syms* symsp, const char* name);
    ~Vpiso___024root();
    VL_UNCOPYABLE(Vpiso___024root);

    // INTERNAL METHODS
    void __Vconfigure(bool first);
} VL_ATTR_ALIGNED(VL_CACHE_LINE_BYTES);


#endif  // guard
