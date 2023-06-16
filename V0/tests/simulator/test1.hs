{-# LANGUAGE DataKinds, TemplateHaskell #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Main where
import Control.Lens hiding (element)
import Control.Lens.TH

import V2H.Simulator.Simulate
import Data.Map qualified as Map
import Data.Set qualified as Set
import Data.Tuple.Extra qualified as Extra
import V2H.IR
import Control.Monad.State.Strict
import Debug.Trace

import GHC.TypeLits
import V2H.IR.DataTypes
import V2H.Simple.IRGenerator.Expressions (mkSignalValueDataObjectFromInteger)
import V2H.Simulator.Signal
clkIRIdentifier = VariableOrNetIdentifierIR "clk"
aIRIdentifier = VariableOrNetIdentifierIR "a"
bIRIdentifier = VariableOrNetIdentifierIR "b"
data ModuleExampleSignals = ModuleExampleCLK | ModuleExampleA | ModuleExampleB | ExampleModuleExampleSubmoduleD | ExampleModuleExampleSubmoduleE

instance FindNetVariableIdentifier (SignalChange ModuleExampleCLK z) where
    fetchNetVariableIdentifier _ = clkIRIdentifier
instance FindNetVariableIdentifier (SignalChange ModuleExampleA a) where
    fetchNetVariableIdentifier _ = aIRIdentifier
instance FindNetVariableIdentifier (SignalChange ModuleExampleB a) where
    fetchNetVariableIdentifier _= bIRIdentifier
instance FindNetVariableIdentifier (SignalChange ExampleModuleExampleSubmoduleD a) where
    fetchNetVariableIdentifier _= bIRIdentifier

data ExampleSubmodule = ExampleSubmodule {
    _d :: SignalChange ExampleModuleExampleSubmoduleD (Signal 1),
    _e :: SignalChange ExampleModuleExampleSubmoduleE (Signal 1)
} deriving (Show)

$(makeLenses ''ExampleSubmodule)

data ExampleModule = ExampleModule {
    _clk :: SignalChange ModuleExampleCLK (Signal 1),
    _a :: SignalChange ModuleExampleA (Signal 1),
    _b :: SignalChange ModuleExampleB (Signal 1),
    _sub :: ExampleSubmodule
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

(<==) :: (FindNetVariableIdentifier (SignalChange a b))
    => Lens' t (SignalChange a b)
    -> b
    -> State (StimulatedCircuit t) ()
(<==) ln newValue = do
    (StimulatedCircuit circuitState changedSignals) <- get
    let b = set (ln . end) newValue circuitState
    put $ StimulatedCircuit b $ Set.insert (fetchNetVariableIdentifier $ view ln circuitState) changedSignals

clkIR = VariableIR clkIRIdentifier $ DTSingular $ STScalar SIVTLogic
aIR = VariableIR aIRIdentifier $ DTSingular $ STScalar SIVTLogic
bIR = VariableIR  bIRIdentifier $ DTSingular $ STScalar SIVTLogic

clkIRConnection = ConnectionVariableIR clkIR Nothing
aIRConnection = ConnectionVariableIR aIR Nothing
bIRConnection = ConnectionVariableIR bIR Nothing

sItems = [NonblockingAssignment bIRConnection $ EUnaryOperator UOExclamationMark $ EConnection aIRConnection]
ir = IR {
    moduleIdentifier = ModuleIdentifierIR "md",
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
    nets = Map.empty,
    submodules = [],
    ports = Map.empty
}
convertToDynamic :: ExampleModule -> ExampleModuleDynamic
convertToDynamic m =
    ExampleModuleDynamic {
        _clkD = SignalDynamic clkIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) $ mkSignalValueDataObjectFromInteger m._clk._end.value,
        _aD = SignalDynamic aIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) $ mkSignalValueDataObjectFromInteger m._a._end.value,
        _bD = SignalDynamic aIRIdentifier $ SignalValue (DTSingular $ STScalar SIVTLogic) $ mkSignalValueDataObjectFromInteger m._b._end.value
    }

convertFromDynamic :: ExampleModuleDynamic -> ExampleModule
convertFromDynamic md =
    ExampleModule {
        _clk = mkUnchanged $ Signal $ signalValueToInteger md._clkD.signalValue (DTSingular $ STScalar SIVTLogic),
        _a = mkUnchanged $ Signal $ signalValueToInteger md._aD.signalValue (DTSingular $ STScalar SIVTLogic),
        _b = mkUnchanged $ Signal $ signalValueToInteger md._bD.signalValue (DTSingular $ STScalar SIVTLogic),
        _sub = ExampleSubmodule {_d = mkUnchanged $ Signal 0, _e = mkUnchanged $ Signal 0}
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

subL = Lens sub
dL = Lens d
main :: IO ()
main =
    let start = ExampleModule {
                    _clk = mkUnchanged $ Signal 0,
                    _a = mkUnchanged $ Signal 0,
                    _b = mkUnchanged $ Signal 0
                }
        toggleClock (clk'::Lens' ExampleModule (SignalChange b' (Signal 1))) a' = do
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