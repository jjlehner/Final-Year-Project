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
import Data.List.Extra qualified as Extra
import Debug.Trace
import Control.Monad
import System.Random
import System.Environment
import Data.Function
import GHC.IO.Handle
import System.IO (stderr)

-- Generate type information for EDSL
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

runTest test startState =
    evalState test $ mkStimulatedCircuit startState

fromBool False = 0
fromBool True = 1

-- Turn an int into a tuple of each bit value
toBinary :: Int -> (Integer,Integer,Integer,Integer,Integer)
toBinary x =
    let testBitX a = fromBool $ testBit x a
    in (testBitX 0,testBitX 1,testBitX 2,testBitX 3,testBitX 4)

isEqualTo ::  Lens' c (SignalChange a (Signal v))-> Signal v -> StateT (StimulatedCircuit c) Identity Bool
isEqualTo ln val = do
    sig <- fetchValue ln
    return (sig == val)


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
            isEqualTo a (Signal a')
                & liftM2 (&&) $ isEqualTo b (Signal b')
                & liftM2 (&&) $ isEqualTo c (Signal c')
                & liftM2 (&&) $ isEqualTo d (Signal d')
                & liftM2 (&&) $ isEqualTo e (Signal e')

    in do
        x <- fmap (read . Extra.headDef "1000") getArgs
        g <- getStdGen
        take x (randomRs (0::Int, 64) g)
            & fmap (\beats -> runTest (test1 beats) initState)
            & and
            & flip assert (hPutStr stderr "All Test Passed")

