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
import Control.Exception (assert)

clkIRIdentifier = VariableOrNetIdentifierIR "clk"
aIRIdentifier = VariableOrNetIdentifierIR "a"
bIRIdentifier = VariableOrNetIdentifierIR "b"
data ModuleExampleSignals = ModuleExampleCLK | ModuleExampleA | ModuleExampleB

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
(<==) :: (FindNetVariableIdentifier s1) => Lens' ExampleModule s1 -> s1 -> State (TestbenchCircuitState ExampleModule) ()
(<==) ln newValue = do
    (TestbenchCircuitState circuitState exampleModule changedSignals) <- get
    let a = view ln exampleModule
    let b = over ln (const newValue) exampleModule
    put $ TestbenchCircuitState b exampleModule $ Set.insert (fetchNetVariableIdentifier a) changedSignals

clkIR = VariableIR clkIRIdentifier LogicIR Nothing Nothing
aIR = VariableIR aIRIdentifier LogicIR Nothing Nothing
bIR = VariableIR  bIRIdentifier LogicIR Nothing Nothing

clkIRConnection = ConnectionVariableIR clkIR Nothing
aIRConnection = ConnectionVariableIR aIR Nothing
bIRConnection = ConnectionVariableIR bIR Nothing

sItems = [NonblockingAssignment bIRConnection $ EConnection aIRConnection]
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
        _clkD = SignalDynamic clkIRIdentifier 1 m._clk.value,
        _aD = SignalDynamic aIRIdentifier 1 m._a.value,
        _bD = SignalDynamic aIRIdentifier 1 m._b.value
    }

convertFromDynamic :: ExampleModuleDynamic -> ExampleModule
convertFromDynamic md =
    ExampleModule {
        _clk = Signal md._clkD.signalValue,
        _a = Signal md._aD.signalValue,
        _b = Signal md._bD.signalValue
    }


evalTestbench :: State (TestbenchCircuitState ExampleModule) ()
evalTestbench = do
    (TestbenchCircuitState testbenchState oldState changingSignals) <- get
    let testbenchStateDynamic = convertToDynamic testbenchState
    let oldStateDynamic = convertToDynamic oldState
    let (TestbenchCircuitState newStateDynamic _ _) =  execState (eval ir lensMap) $ TestbenchCircuitState testbenchStateDynamic oldStateDynamic changingSignals
    let newState = convertFromDynamic newStateDynamic
    put $ TestbenchCircuitState newState newState Set.empty
    return ()

main :: IO ()
main =
    let start = ExampleModule {
                    _clk = Signal 0,
                    _a = Signal 1,
                    _b = Signal 0
                }
        test1 = do
            clk <== Signal 1
            evalTestbench
            clk <== Signal 0
            a <== Signal 0
            evalTestbench
            clk <== Signal 1
            evalTestbench
        (TestbenchCircuitState end _ b) = execState test1 (TestbenchCircuitState start start Set.empty)
    in
        print $ assert False end




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