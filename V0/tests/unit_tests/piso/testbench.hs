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
import Data.Bits
import Debug.Trace
import Control.Monad
import Data.Function
import System.Random
import System.Environment
import System.IO
import Data.List.Extra qualified as Extra

$(setup "piso" [
        "tests/unit_tests/piso/rtl/piso.sv",
        "tests/unit_tests/piso/rtl/stager.sv"
    ])

$(makeFieldsNoPrefix ''Piso)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

mAssert :: (Monad m) => Bool -> a -> m a
mAssert a b = assert a (pure b)


runTestGetValue test startState =
    evalState test $ mkStimulatedCircuit startState

fromBool :: Bool -> Integer
fromBool False = 0
fromBool True = 1

fromBinary x =
    zip [0..] (reverse x)
    & fmap (\(i,b) -> 2^i * b)
    & sum

toggleClk = do
    clk <== mkSignal @0
    eval'
    clk <== mkSignal @1
    eval'

isEqualTo ::  Lens' c (SignalChange a (Signal v))-> Signal v -> err -> StateT (StimulatedCircuit c) Identity err
isEqualTo ln val errMsg = do
    sig <- fetchValue ln
    assert (sig == val) $ pure errMsg

saveOutThenClk :: State (StimulatedCircuit Piso) (Signal 1)
saveOutThenClk = do
    uptake <== mkSignal @0
    enable <== mkSignal @1
    eval'
    a <- fetchValue out
    toggleClk
    return a

shiftOutAll :: State (StimulatedCircuit Piso) [Signal 1]
shiftOutAll = do
    replicateM 6 saveOutThenClk

setInput inVal = do
    enable <== mkSignal @1
    uptake <== mkSignal @1
    eval'
    in0 <== Signal (fromBool $ testBit inVal 0)
    in1 <== Signal (fromBool $ testBit inVal 1)
    in2 <== Signal (fromBool $ testBit inVal 2)
    in3 <== Signal (fromBool $ testBit inVal 3)
    in4 <== Signal (fromBool $ testBit inVal 4)
    in5 <== Signal (fromBool $ testBit inVal 5)
    toggleClk
    return ()

main :: IO ()
main =
    let test1 x = do
            let xMod = x `mod` 2 ^ 6
            setInput xMod
            a <- fmap (fromBinary . fmap (value)) shiftOutAll
            return $ a == xMod

        runTest x = runTestGetValue (test1 x) initState
    in do
        x <- fmap (read . Extra.headDef "1000") getArgs
        g <- getStdGen
        take x (randomRs (0::Integer, 64) g)
            & fmap runTest
            & and
            & flip assert (hPutStr stderr $ "All Test Passed, " ++ show x ++ ", ")