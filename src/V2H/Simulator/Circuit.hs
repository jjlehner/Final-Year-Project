module V2H.Simulator.Circuit where
import Data.Map qualified as Map
import V2H.Simulator.Signal
import V2H.IR
type DynamicCircuitState = Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) SignalValue
