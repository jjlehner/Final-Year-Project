{-# LANGUAGE TemplateHaskell, DataKinds, FunctionalDependencies #-}

module Main where
import V2H.Simulator.Signal
import V2H.Simulator.Simulate
import V2H.V2H
import V2H.IRGenerator

import V2H.Simulator.EDSL
import Control.Lens.Combinators
import Control.Monad.State.Strict
import Text.Pretty.Simple
import Data.Set qualified as Set

import Debug.Trace
import Control.Monad
$(setup "alu" [
        "tests/unit_tests/alu/rtl/alu.sv"
    ])

$(makeFieldsNoPrefix ''Alu)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x
toggleClk = do
            clk <== mkSignal @0
            eval'
            clk <== mkSignal @1
            eval'
e = convertFromDynamic empty
runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState
main :: IO ()
main =
    let test1 = do
            a <== mkSignal @2
            b <== mkSignal @3
            operation <== mkSignal @0
            toggleClk
            o1 <- fetchValue out
            assert (o1 == mkSignal @0) $ pure ()
            a <== mkSignal @1
            b <== mkSignal @2
            operation <== mkSignal @2
            toggleClk
            o2 <- fetchValue out
            assert (o2 == mkSignal @2) $ pure ()
            a <== mkSignal @2
            b <== mkSignal @0
            operation <== mkSignal @2
            toggleClk
            o2 <- fetchValue out
            assert (o2 == mkSignal @0) $ pure ()
            pure ()
    in do
        pPrint expandedIR
        pPrint $ runTest (replicateM_ 1 test1) e
