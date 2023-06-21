// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Model implementation (design independent parts)

#include "Vpiso.h"
#include "Vpiso__Syms.h"

//============================================================
// Constructors

Vpiso::Vpiso(VerilatedContext* _vcontextp__, const char* _vcname__)
    : VerilatedModel{*_vcontextp__}
    , vlSymsp{new Vpiso__Syms(contextp(), _vcname__, this)}
    , clk{vlSymsp->TOP.clk}
    , uptake{vlSymsp->TOP.uptake}
    , enable{vlSymsp->TOP.enable}
    , in0{vlSymsp->TOP.in0}
    , in1{vlSymsp->TOP.in1}
    , in2{vlSymsp->TOP.in2}
    , in3{vlSymsp->TOP.in3}
    , in4{vlSymsp->TOP.in4}
    , in5{vlSymsp->TOP.in5}
    , out{vlSymsp->TOP.out}
    , rootp{&(vlSymsp->TOP)}
{
    // Register model with the context
    contextp()->addModel(this);
}

Vpiso::Vpiso(const char* _vcname__)
    : Vpiso(Verilated::threadContextp(), _vcname__)
{
}

//============================================================
// Destructor

Vpiso::~Vpiso() {
    delete vlSymsp;
}

//============================================================
// Evaluation function

#ifdef VL_DEBUG
void Vpiso___024root___eval_debug_assertions(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG
void Vpiso___024root___eval_static(Vpiso___024root* vlSelf);
void Vpiso___024root___eval_initial(Vpiso___024root* vlSelf);
void Vpiso___024root___eval_settle(Vpiso___024root* vlSelf);
void Vpiso___024root___eval(Vpiso___024root* vlSelf);

void Vpiso::eval_step() {
    VL_DEBUG_IF(VL_DBG_MSGF("+++++TOP Evaluate Vpiso::eval_step\n"); );
#ifdef VL_DEBUG
    // Debug assertions
    Vpiso___024root___eval_debug_assertions(&(vlSymsp->TOP));
#endif  // VL_DEBUG
    if (VL_UNLIKELY(!vlSymsp->__Vm_didInit)) {
        vlSymsp->__Vm_didInit = true;
        VL_DEBUG_IF(VL_DBG_MSGF("+ Initial\n"););
        Vpiso___024root___eval_static(&(vlSymsp->TOP));
        Vpiso___024root___eval_initial(&(vlSymsp->TOP));
        Vpiso___024root___eval_settle(&(vlSymsp->TOP));
    }
    VL_DEBUG_IF(VL_DBG_MSGF("+ Eval\n"););
    Vpiso___024root___eval(&(vlSymsp->TOP));
    // Evaluate cleanup
}

//============================================================
// Events and timing
bool Vpiso::eventsPending() { return false; }

uint64_t Vpiso::nextTimeSlot() {
    VL_FATAL_MT(__FILE__, __LINE__, "", "%Error: No delays in the design");
    return 0;
}

//============================================================
// Utilities

const char* Vpiso::name() const {
    return vlSymsp->name();
}

//============================================================
// Invoke final blocks

void Vpiso___024root___eval_final(Vpiso___024root* vlSelf);

VL_ATTR_COLD void Vpiso::final() {
    Vpiso___024root___eval_final(&(vlSymsp->TOP));
}

//============================================================
// Implementations of abstract methods from VerilatedModel

const char* Vpiso::hierName() const { return vlSymsp->name(); }
const char* Vpiso::modelName() const { return "Vpiso"; }
unsigned Vpiso::threads() const { return 1; }
