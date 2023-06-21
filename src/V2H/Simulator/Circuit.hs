module V2H.Simulator.Circuit where
import Data.Map qualified as Map
import V2H.Simulator.Signal
import V2H.IR

-- | Dynamic representation of circuit used within Simulator
type DynamicCircuitState = Map.Map (HierarchicalIdentifierIR VariableOrNetIdentifierIR) SignalValue
