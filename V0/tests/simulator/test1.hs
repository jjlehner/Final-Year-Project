{-# LANGUAGE DataKinds, TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where
import Control.Lens hiding (element)
import Control.Lens.TH

import V2H.Simulator.Simulate
import Data.Map qualified as Map
import Data.Set qualified as Set
import V2H.IR
import Control.Monad.State.Strict
import Debug.Trace

import GHC.TypeLits
import V2H.IR.DataTypes

clkIRIdentifier = VariableOrNetIdentifierIR "clk"
aIRIdentifier = VariableOrNetIdentifierIR "a"
bIRIdentifier = VariableOrNetIdentifierIR "b"
data ModuleExampleSignals = ModuleExampleCLK | ModuleExampleA | ModuleExampleB

newtype Signal sig (a::Nat) = Signal {value::Integer} deriving (Show, Eq)

instance FindNetVariableIdentifier (Signal ModuleExampleCLK a) where
    fetchNetVariableIdentifier _ = clkIRIdentifier
instance FindNetVariableIdentifier (Signal ModuleExampleA a) where
    fetchNetVariableIdentifier _ = aIRIdentifier
instance FindNetVariableIdentifier (Signal ModuleExampleB a) where
    fetchNetVariableIdentifier _ = bIRIdentifier

data ExampleModule = ExampleModule {
    _clk :: Signal ModuleExampleCLK 1,
    _a :: Signal ModuleExampleA 1,
    _b :: Signal ModuleExampleB 1
} deriving (Show)

$(makeLenses ''ExampleModule)

data ExampleModuleDynamic = ExampleModuleDynamic {
    _clkD :: SignalDynamic,
    _aD :: SignalDynamic,
    _bD :: SignalDynamic
} deriving (Show)

$(makeLenses ''ExampleModuleDynamic)

lensMap :: Map.Map VariableOrNetIdentifierIR (ReifiedLens' ExampleModuleDynamic SignalDynamic)
lensMap = Map.fromList [(clkIRIdentifier, Lens clkD), (aIRIdentifier,Lens aD), (bIRIdentifier, Lens bD)]

x = ExampleModule $ Signal 0
-- (<=) :: a -> b ->
(<==) :: (FindNetVariableIdentifier s1) => Lens' ExampleModule s1 -> s1 -> State (StimulatedCircuit ExampleModule) ()
(<==) ln newValue = do
    (StimulatedCircuit circuitState changedSignals) <- get
    let b = set ln newValue circuitState
    put $ StimulatedCircuit b $ Set.insert (fetchNetVariableIdentifier newValue) changedSignals

clkIR = VariableIR clkIRIdentifier $ DTSingular $ STScalar SIVTLogic
aIR = VariableIR aIRIdentifier $ DTSingular $ STScalar SIVTLogic
bIR = VariableIR  bIRIdentifier $ DTSingular $ STScalar SIVTLogic

clkIRConnection = ConnectionVariableIR clkIR Nothing
aIRConnection = ConnectionVariableIR aIR Nothing
bIRConnection = ConnectionVariableIR bIR Nothing

sItems = [NonblockingAssignment bIRConnection $ EUnaryOperator UOExclamationMark $ EConnection aIRConnection]
ir = IR {
    alwaysConstructs = Map.fromList [(  AlwaysConstructIdentifierIR "ExampleModuleAlways1",
                                        AlwaysConstructIR {
                                            identifier                              = AlwaysConstructIdentifierIR "ExampleModuleAlways1",
                                            sensitivity                             = FF $ Set.singleton $ EventExpressionIR {connection=clkIRConnection, edgeIdentifier=Just Posedge},
                                            inputConnections                        = Set.singleton clkIRConnection,
                                            outputConnections                       = Set.singleton aIRConnection,
                                            statementItems                          = sItems
                                        })],
    variables = Map.fromList $ [    (clkIRIdentifier, clkIR),
                                    (aIRIdentifier, aIR),
                                    (bIRIdentifier, bIR)    ],
    nets = Map.empty
}
convertToDynamic :: ExampleModule -> ExampleModuleDynamic
convertToDynamic m =
    ExampleModuleDynamic {
        _clkD = SignalDynamic clkIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) m._clk.value,
        _aD = SignalDynamic aIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) m._a.value,
        _bD = SignalDynamic aIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) m._b.value
    }

convertFromDynamic :: ExampleModuleDynamic -> ExampleModule
convertFromDynamic md =
    ExampleModule {
        _clk = Signal $ signalValueToInteger md._clkD.signalValue,
        _a = Signal $ signalValueToInteger md._aD.signalValue,
        _b = Signal $ signalValueToInteger md._bD.signalValue
    }


evalTestbench :: State (StimulatedCircuit ExampleModule) ()
evalTestbench = do
    (StimulatedCircuit testbenchState changingSignals) <- get
    let testbenchStateDynamic = convertToDynamic testbenchState
    let (StimulatedCircuit newStateDynamic _) =  execState (eval ir lensMap) $ StimulatedCircuit testbenchStateDynamic changingSignals
    let newState = convertFromDynamic newStateDynamic
    put $ StimulatedCircuit newState Set.empty

runTest test startState =
    _stimulatedState $ execState test $ mkStimulatedCircuit startState


main :: IO ()
main =
    let start = ExampleModule {
                    _clk = Signal 0,
                    _a = Signal 0,
                    _b = Signal 1
                }
        toggleClock (clk'::Lens' ExampleModule (Signal b' 1)) a' = do
            clk' <== Signal 0
            evalTestbench
            a'
            evalTestbench
            clk' <== Signal 1
            evalTestbench


        test1 = do
            toggleClock clk (a <== Signal 1)
            toggleClock clk (a <== Signal 0)
            -- toggleClock clk (a <== Signal 1)

    in
        print $ runTest test1 start




{-
    module ExampleModule (
        input    logic       clk,
        input    logic       a,
        output   logic       b
    );
        always_ff@(posedge clk) begin
            b <= a;
        end
    endmodule -}