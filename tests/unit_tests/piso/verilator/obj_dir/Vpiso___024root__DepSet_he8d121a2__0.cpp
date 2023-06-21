// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vpiso.h for the primary calling header

#include "verilated.h"

#include "Vpiso__Syms.h"
#include "Vpiso___024root.h"

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__ico(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG

void Vpiso___024root___eval_triggers__ico(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_triggers__ico\n"); );
    // Body
    vlSelf->__VicoTriggered.at(0U) = (0U == vlSelf->__VicoIterCount);
#ifdef VL_DEBUG
    if (VL_UNLIKELY(vlSymsp->_vm_contextp__->debug())) {
        Vpiso___024root___dump_triggers__ico(vlSelf);
    }
#endif
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__act(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG

void Vpiso___024root___eval_triggers__act(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_triggers__act\n"); );
    // Body
    vlSelf->__VactTriggered.at(0U) = ((IData)(vlSelf->clk) 
                                      & (~ (IData)(vlSelf->__Vtrigrprev__TOP__clk)));
    vlSelf->__Vtrigrprev__TOP__clk = vlSelf->clk;
#ifdef VL_DEBUG
    if (VL_UNLIKELY(vlSymsp->_vm_contextp__->debug())) {
        Vpiso___024root___dump_triggers__act(vlSelf);
    }
#endif
}
