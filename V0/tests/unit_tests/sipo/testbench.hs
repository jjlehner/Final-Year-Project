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
import Data.Bits
import Debug.Trace
import Control.Monad
import Data.Function
import System.Random
import System.Environment
import System.IO
import Data.List.Extra qualified as Extra

$(setup "sipo" [
        "tests/unit_tests/sipo/rtl/sipo.sv",
        "tests/unit_tests/sipo/rtl/stager.sv"
    ])

$(makeFieldsNoPrefix ''Sipo)

runTestGetValue test startState =
    evalState test $ mkStimulatedCircuit startState

toggleClk = do
    clk <== mkSignal @0
    eval'
    clk <== mkSignal @1
    eval'

shiftBit x = do
    enable <== mkSignal @1
    eval'
    serialIn <== Signal (x .&. 1)
    toggleClk
    return ()

toBinary 0 _ = []
toBinary width num =
    (num .&. 1) : toBinary (width - 1) (shift num (-1))

shiftNibble x = do
    replicateM_ 4 $ forM_ (toBinary 4 x) shiftBit

getNibbleOut :: State (StimulatedCircuit Sipo) Integer
getNibbleOut = do
    value <$> fetchValue parallelOut

main :: IO ()
main =
    let test1 x = do
                    let xMod = x `mod` 2 ^4
                    shiftNibble x
                    a <- getNibbleOut
                    traceShow a $ pure ()
                    return $ fromInteger a == xMod
    in print $ runTestGetValue (test1 12) initState