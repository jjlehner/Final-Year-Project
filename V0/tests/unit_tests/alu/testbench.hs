{-# LANGUAGE TemplateHaskell, DataKinds, FunctionalDependencies #-}

module Main where
import V2H.Simulator.Signal
import V2H.Simulator.Simulate
import V2H.Simple.V2H
import V2H.Simple.IRGenerator

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
            toggleClk
            out <- fetchValue out
            pure ()
            -- assert (out == mkSignal @9) $ pure ()
    in do
        pPrint expandedIR
        pPrint $ runTest (replicateM_ 1 test1) e
