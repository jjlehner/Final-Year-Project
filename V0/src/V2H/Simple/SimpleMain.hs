{-# LANGUAGE TemplateHaskell, DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}
module Main where
import V2H.Simulator.Signal
import V2H.Simulator.Simulate
import V2H.Simple.V2H
import V2H.Simple.IRGenerator
import Control.Lens.Combinators
import Control.Monad.State.Strict
import Text.Pretty.Simple
import Data.Set qualified as Set
$(setup "top" [
        "tests/parser/simple/empty_module.sv",
        "tests/parser/simple/submodule.sv",
        "tests/parser/simple/sub_sub_module.sv"
    ])

$(makeFieldsNoPrefix ''Top)
$(makeFieldsNoPrefix ''TopX)
$(makeFieldsNoPrefix ''TopW)

b = convertFromDynamic empty
runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState

evalTestbench = do
    (StimulatedCircuit testbenchState changingSignals) <- get
    let testbenchStateDynamic = convertToDynamic testbenchState
    let (StimulatedCircuit newStateDynamic _) =  execState (eval expandedIR) $ StimulatedCircuit testbenchStateDynamic changingSignals
    let newState = convertFromDynamic newStateDynamic
    put $ StimulatedCircuit newState Set.empty

main =
    let toggleClock (clk'::Lens' a (SignalChange b' (Signal c)))  a' = do
            clk' <== Signal 0
            evalTestbench
            a'
            evalTestbench
            clk' <== Signal 1
            evalTestbench

        test1 = do
            toggleClock clk (a <== Signal 1)
            toggleClock clk (a <== Signal 0)
            toggleClock clk (a <== Signal 1)
    in
        print $ runTest test1 b