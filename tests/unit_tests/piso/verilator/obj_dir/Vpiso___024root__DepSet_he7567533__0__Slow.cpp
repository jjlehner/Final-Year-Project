// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vpiso.h for the primary calling header

#include "verilated.h"

#include "Vpiso___024root.h"

VL_ATTR_COLD void Vpiso___024root___eval_static(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_static\n"); );
}

VL_ATTR_COLD void Vpiso___024root___eval_initial(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_initial\n"); );
    // Body
    vlSelf->__Vtrigrprev__TOP__clk = vlSelf->clk;
}

VL_ATTR_COLD void Vpiso___024root___eval_final(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_final\n"); );
}

VL_ATTR_COLD void Vpiso___024root___eval_triggers__stl(Vpiso___024root* vlSelf);
#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__stl(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___eval_stl(Vpiso___024root* vlSelf);

VL_ATTR_COLD void Vpiso___024root___eval_settle(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_settle\n"); );
    // Init
    CData/*0:0*/ __VstlContinue;
    // Body
    vlSelf->__VstlIterCount = 0U;
    __VstlContinue = 1U;
    while (__VstlContinue) {
        __VstlContinue = 0U;
        Vpiso___024root___eval_triggers__stl(vlSelf);
        if (vlSelf->__VstlTriggered.any()) {
            __VstlContinue = 1U;
            if ((0x64U < vlSelf->__VstlIterCount)) {
#ifdef VL_DEBUG
                Vpiso___024root___dump_triggers__stl(vlSelf);
#endif
                VL_FATAL_MT("../rtl/piso.sv", 1, "", "Settle region did not converge.");
            }
            vlSelf->__VstlIterCount = ((IData)(1U) 
                                       + vlSelf->__VstlIterCount);
            Vpiso___024root___eval_stl(vlSelf);
        }
    }
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__stl(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___dump_triggers__stl\n"); );
    // Body
    if ((1U & (~ (IData)(vlSelf->__VstlTriggered.any())))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if (vlSelf->__VstlTriggered.at(0U)) {
        VL_DBG_MSGF("         'stl' region trigger index 0 is active: Internal 'stl' trigger - first iteration\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void Vpiso___024root___stl_sequent__TOP__0(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___stl_sequent__TOP__0\n"); );
    // Body
    vlSelf->out = vlSelf->piso__DOT__a6;
    if (vlSelf->enable) {
        if (vlSelf->uptake) {
            vlSelf->piso__DOT__s0__DOT__nextprime = vlSelf->in0;
            vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->in1;
            vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->in2;
            vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->in3;
            vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->in4;
            vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->in5;
        } else {
            vlSelf->piso__DOT__s0__DOT__nextprime = 0U;
            vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->piso__DOT__a1;
            vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->piso__DOT__a2;
            vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->piso__DOT__a3;
            vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->piso__DOT__a4;
            vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->piso__DOT__a5;
        }
    } else {
        vlSelf->piso__DOT__s0__DOT__nextprime = vlSelf->piso__DOT__a1;
        vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->piso__DOT__a2;
        vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->piso__DOT__a3;
        vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->piso__DOT__a4;
        vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->piso__DOT__a5;
        vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->piso__DOT__a6;
    }
}

VL_ATTR_COLD void Vpiso___024root___eval_stl(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_stl\n"); );
    // Body
    if (vlSelf->__VstlTriggered.at(0U)) {
        Vpiso___024root___stl_sequent__TOP__0(vlSelf);
    }
}

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__ico(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___dump_triggers__ico\n"); );
    // Body
    if ((1U & (~ (IData)(vlSelf->__VicoTriggered.any())))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if (vlSelf->__VicoTriggered.at(0U)) {
        VL_DBG_MSGF("         'ico' region trigger index 0 is active: Internal 'ico' trigger - first iteration\n");
    }
}
#endif  // VL_DEBUG

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__act(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___dump_triggers__act\n"); );
    // Body
    if ((1U & (~ (IData)(vlSelf->__VactTriggered.any())))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if (vlSelf->__VactTriggered.at(0U)) {
        VL_DBG_MSGF("         'act' region trigger index 0 is active: @(posedge clk)\n");
    }
}
#endif  // VL_DEBUG

#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__nba(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___dump_triggers__nba\n"); );
    // Body
    if ((1U & (~ (IData)(vlSelf->__VnbaTriggered.any())))) {
        VL_DBG_MSGF("         No triggers active\n");
    }
    if (vlSelf->__VnbaTriggered.at(0U)) {
        VL_DBG_MSGF("         'nba' region trigger index 0 is active: @(posedge clk)\n");
    }
}
#endif  // VL_DEBUG

VL_ATTR_COLD void Vpiso___024root___ctor_var_reset(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___ctor_var_reset\n"); );
    // Body
    vlSelf->clk = VL_RAND_RESET_I(1);
    vlSelf->uptake = VL_RAND_RESET_I(1);
    vlSelf->enable = VL_RAND_RESET_I(1);
    vlSelf->in0 = VL_RAND_RESET_I(1);
    vlSelf->in1 = VL_RAND_RESET_I(1);
    vlSelf->in2 = VL_RAND_RESET_I(1);
    vlSelf->in3 = VL_RAND_RESET_I(1);
    vlSelf->in4 = VL_RAND_RESET_I(1);
    vlSelf->in5 = VL_RAND_RESET_I(1);
    vlSelf->out = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a1 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a2 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a3 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a4 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a5 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__a6 = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s0__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s1__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s2__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s3__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s4__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->piso__DOT__s5__DOT__nextprime = VL_RAND_RESET_I(1);
    vlSelf->__VstlIterCount = 0;
    vlSelf->__VicoIterCount = 0;
    vlSelf->__Vtrigrprev__TOP__clk = VL_RAND_RESET_I(1);
    vlSelf->__VactIterCount = 0;
    vlSelf->__VactContinue = 0;
}
