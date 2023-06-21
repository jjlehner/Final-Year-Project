// Verilated -*- C++ -*-
// DESCRIPTION: Verilator output: Design implementation internals
// See Vpiso.h for the primary calling header

#include "verilated.h"

#include "Vpiso___024root.h"

VL_INLINE_OPT void Vpiso___024root___ico_sequent__TOP__0(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___ico_sequent__TOP__0\n"); );
    // Body
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

void Vpiso___024root___eval_ico(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_ico\n"); );
    // Body
    if (vlSelf->__VicoTriggered.at(0U)) {
        Vpiso___024root___ico_sequent__TOP__0(vlSelf);
    }
}

void Vpiso___024root___eval_act(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_act\n"); );
}

VL_INLINE_OPT void Vpiso___024root___nba_sequent__TOP__0(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___nba_sequent__TOP__0\n"); );
    // Body
    vlSelf->piso__DOT__a6 = vlSelf->piso__DOT__s5__DOT__nextprime;
    vlSelf->piso__DOT__a1 = vlSelf->piso__DOT__s0__DOT__nextprime;
    vlSelf->piso__DOT__a5 = vlSelf->piso__DOT__s4__DOT__nextprime;
    vlSelf->piso__DOT__a4 = vlSelf->piso__DOT__s3__DOT__nextprime;
    vlSelf->piso__DOT__a3 = vlSelf->piso__DOT__s2__DOT__nextprime;
    vlSelf->piso__DOT__a2 = vlSelf->piso__DOT__s1__DOT__nextprime;
    vlSelf->out = vlSelf->piso__DOT__a6;
    if (vlSelf->enable) {
        if (vlSelf->uptake) {
            vlSelf->piso__DOT__s0__DOT__nextprime = vlSelf->in0;
            vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->in5;
            vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->in4;
            vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->in3;
            vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->in1;
            vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->in2;
        } else {
            vlSelf->piso__DOT__s0__DOT__nextprime = 0U;
            vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->piso__DOT__a5;
            vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->piso__DOT__a4;
            vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->piso__DOT__a3;
            vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->piso__DOT__a1;
            vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->piso__DOT__a2;
        }
    } else {
        vlSelf->piso__DOT__s0__DOT__nextprime = vlSelf->piso__DOT__a1;
        vlSelf->piso__DOT__s5__DOT__nextprime = vlSelf->piso__DOT__a6;
        vlSelf->piso__DOT__s4__DOT__nextprime = vlSelf->piso__DOT__a5;
        vlSelf->piso__DOT__s3__DOT__nextprime = vlSelf->piso__DOT__a4;
        vlSelf->piso__DOT__s1__DOT__nextprime = vlSelf->piso__DOT__a2;
        vlSelf->piso__DOT__s2__DOT__nextprime = vlSelf->piso__DOT__a3;
    }
}

void Vpiso___024root___eval_nba(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_nba\n"); );
    // Body
    if (vlSelf->__VnbaTriggered.at(0U)) {
        Vpiso___024root___nba_sequent__TOP__0(vlSelf);
    }
}

void Vpiso___024root___eval_triggers__ico(Vpiso___024root* vlSelf);
#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__ico(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG
void Vpiso___024root___eval_triggers__act(Vpiso___024root* vlSelf);
#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__act(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG
#ifdef VL_DEBUG
VL_ATTR_COLD void Vpiso___024root___dump_triggers__nba(Vpiso___024root* vlSelf);
#endif  // VL_DEBUG

void Vpiso___024root___eval(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval\n"); );
    // Init
    CData/*0:0*/ __VicoContinue;
    VlTriggerVec<1> __VpreTriggered;
    IData/*31:0*/ __VnbaIterCount;
    CData/*0:0*/ __VnbaContinue;
    // Body
    vlSelf->__VicoIterCount = 0U;
    __VicoContinue = 1U;
    while (__VicoContinue) {
        __VicoContinue = 0U;
        Vpiso___024root___eval_triggers__ico(vlSelf);
        if (vlSelf->__VicoTriggered.any()) {
            __VicoContinue = 1U;
            if ((0x64U < vlSelf->__VicoIterCount)) {
#ifdef VL_DEBUG
                Vpiso___024root___dump_triggers__ico(vlSelf);
#endif
                VL_FATAL_MT("../rtl/piso.sv", 1, "", "Input combinational region did not converge.");
            }
            vlSelf->__VicoIterCount = ((IData)(1U) 
                                       + vlSelf->__VicoIterCount);
            Vpiso___024root___eval_ico(vlSelf);
        }
    }
    __VnbaIterCount = 0U;
    __VnbaContinue = 1U;
    while (__VnbaContinue) {
        __VnbaContinue = 0U;
        vlSelf->__VnbaTriggered.clear();
        vlSelf->__VactIterCount = 0U;
        vlSelf->__VactContinue = 1U;
        while (vlSelf->__VactContinue) {
            vlSelf->__VactContinue = 0U;
            Vpiso___024root___eval_triggers__act(vlSelf);
            if (vlSelf->__VactTriggered.any()) {
                vlSelf->__VactContinue = 1U;
                if ((0x64U < vlSelf->__VactIterCount)) {
#ifdef VL_DEBUG
                    Vpiso___024root___dump_triggers__act(vlSelf);
#endif
                    VL_FATAL_MT("../rtl/piso.sv", 1, "", "Active region did not converge.");
                }
                vlSelf->__VactIterCount = ((IData)(1U) 
                                           + vlSelf->__VactIterCount);
                __VpreTriggered.andNot(vlSelf->__VactTriggered, vlSelf->__VnbaTriggered);
                vlSelf->__VnbaTriggered.set(vlSelf->__VactTriggered);
                Vpiso___024root___eval_act(vlSelf);
            }
        }
        if (vlSelf->__VnbaTriggered.any()) {
            __VnbaContinue = 1U;
            if ((0x64U < __VnbaIterCount)) {
#ifdef VL_DEBUG
                Vpiso___024root___dump_triggers__nba(vlSelf);
#endif
                VL_FATAL_MT("../rtl/piso.sv", 1, "", "NBA region did not converge.");
            }
            __VnbaIterCount = ((IData)(1U) + __VnbaIterCount);
            Vpiso___024root___eval_nba(vlSelf);
        }
    }
}

#ifdef VL_DEBUG
void Vpiso___024root___eval_debug_assertions(Vpiso___024root* vlSelf) {
    if (false && vlSelf) {}  // Prevent unused
    Vpiso__Syms* const __restrict vlSymsp VL_ATTR_UNUSED = vlSelf->vlSymsp;
    VL_DEBUG_IF(VL_DBG_MSGF("+    Vpiso___024root___eval_debug_assertions\n"); );
    // Body
    if (VL_UNLIKELY((vlSelf->clk & 0xfeU))) {
        Verilated::overWidthError("clk");}
    if (VL_UNLIKELY((vlSelf->uptake & 0xfeU))) {
        Verilated::overWidthError("uptake");}
    if (VL_UNLIKELY((vlSelf->enable & 0xfeU))) {
        Verilated::overWidthError("enable");}
    if (VL_UNLIKELY((vlSelf->in0 & 0xfeU))) {
        Verilated::overWidthError("in0");}
    if (VL_UNLIKELY((vlSelf->in1 & 0xfeU))) {
        Verilated::overWidthError("in1");}
    if (VL_UNLIKELY((vlSelf->in2 & 0xfeU))) {
        Verilated::overWidthError("in2");}
    if (VL_UNLIKELY((vlSelf->in3 & 0xfeU))) {
        Verilated::overWidthError("in3");}
    if (VL_UNLIKELY((vlSelf->in4 & 0xfeU))) {
        Verilated::overWidthError("in4");}
    if (VL_UNLIKELY((vlSelf->in5 & 0xfeU))) {
        Verilated::overWidthError("in5");}
}
#endif  // VL_DEBUG
