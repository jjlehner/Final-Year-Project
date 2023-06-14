{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
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
$(setup "flipper" [
        "tests/parser/simple/flipper.sv"
    ])
$(makeFieldsNoPrefix ''Flipper)

b = convertFromDynamic empty
runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState

--- >>> :t d
-- d :: HasD s a => Lens' s a
assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

main :: IO ()
main =
    let toggleClock' (clk'::Lens' a (SignalChange b' (Signal c)))  a' = do
            clk' <== Signal 0
            eval'
            a'
            eval'
            clk' <== Signal 1
            eval'

        toggleClk = do
            clk <== mkSignal @0
            eval'
            clk <== mkSignal @1
            eval'



        test1 = do
            initialDValue <- fetchValue d
            toggleClk
            finalDValue <- fetchValue d
            assert (finalDValue /= initialDValue) $ pure ()
    in do
        pPrint $ runTest (replicateM_ 10 test1) b
