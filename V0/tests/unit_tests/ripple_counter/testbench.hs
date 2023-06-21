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

$(setup "top" [
        "tests/unit_tests/ripple_counter/rtl/ripple.sv",
        "tests/unit_tests/ripple_counter/rtl/top.sv"
    ])

$(makeFieldsNoPrefix ''Top)

assert :: Bool -> a -> a
assert False x = error "assertion failed!"
assert _     x = x

mAssert :: (Monad m) => Bool -> a -> m a
mAssert a b = assert a (pure b)

empty' = convertFromDynamic $ triggerAllAlwaysCombBlocks expandedIR empty
runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState

fromBool False = 0
fromBool True = 1

toBinary :: Int -> (Integer,Integer,Integer,Integer,Integer)
toBinary x =
    let testBitX a = fromBool $ testBit x a
    in (testBitX 0,testBitX 1,testBitX 2,testBitX 3,testBitX 4)

isEqualTo ::  Lens' c (SignalChange a (Signal v))-> Signal v -> err -> StateT (StimulatedCircuit c) Identity err
isEqualTo ln val errMsg = do
    sig <- fetchValue ln
    assert (sig == val) $ pure errMsg


main :: IO ()
main =
    let toggleClk = do
            clk <== mkSignal @0
            eval'
            clk <== mkSignal @1
            eval'

        test1 x = do
            replicateM_ x toggleClk
            let (a',b',c',d',e') = toBinary (x-1)
            isEqualTo a (Signal a') "A did not match"
            isEqualTo b (Signal b') "B did not match"
            isEqualTo c (Signal c') "C did not match"
            isEqualTo d (Signal d') "D did not match"
            isEqualTo e (Signal e') "E did not match"

    in do
        pPrint empty'
        let x= runTest (test1 100000) empty'
        putStrLn (show x._a._start.value ++ " "++ show x._b._start.value ++ " " ++ show x._c._start.value ++ " " ++ show x._d._start.value ++ " " ++ show x._e._start.value)

