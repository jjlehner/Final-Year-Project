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
$(setup "flipper" [
        "tests/unit_tests/flipper/rtl/flipper.sv"
    ])

$(setupWriteOutput "tests/unit_tests/flipper/rtl/flipper_gen.txt" "flipper" [
        "tests/unit_tests/flipper/rtl/flipper.sv"
    ])

$(makeFieldsNoPrefix ''Flipper)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

b = convertFromDynamic empty
runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState

main :: IO ()
main =
    let toggleClk = do
            clk <== mkSignal @0
            eval'
            clk <== mkSignal @1
            eval'

        test1 = do
            initialDValue <- fetchValue d -- Fetch initial value
            toggleClk
            finalDValue <- fetchValue d -- Fetch final value
            assert (finalDValue /= initialDValue) $ pure () -- Check Final value does not equal initial value
    in do
        pPrint $ runTest (replicateM_ 100 test1) b -- Run the test 100 times
